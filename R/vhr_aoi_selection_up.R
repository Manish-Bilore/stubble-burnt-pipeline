## ===================================================================
## vhr_aoi_selection_up.R
## Select UP42 WorldView Legion AOIs for validating the Uttar Pradesh
## Rabi 2026 burnt-area product.
##
## Runs on the GCP VM against /data/stubble_burnt.
##
## Differs from the Haryana vhr_dnbr_overlap.R / vhr_validation_design.R
## in three ways that matter:
##   1. UP spans MGRS zones 43/44/45 - tile regex and footprint handling
##      are zone-agnostic, and all geometry is reconciled in one CRS.
##   2. Scenes are scored not only for overlap and temporal bracketing
##      but for COMMISSION-ERROR DIAGNOSTIC VALUE: the v6 product is
##      known to over-detect (harvest/senescence, not char), so the most
##      useful scenes are those covering large flagged areas where VHR
##      can adjudicate burned vs merely-cleared.
##   3. Division attribution (from the corrected admin GeoPackage) is
##      carried through so the final selection can be spread across UP
##      rather than clustering in whichever division has most scenes.
##
## Outputs -> AOI_selection/outputs/
##   dnbr_tile_inventory.csv     per-tile date coverage
##   vhr_tile_bracket.csv        scene x tile spatial + temporal match
##   vhr_scene_scored.csv        one row per scene, ranked
##   vhr_selected_scenes.gpkg    geometry of the shortlist
##   vhr_selection_map.png       footprints vs districts vs divisions
## ===================================================================

suppressPackageStartupMessages({
  library(sf); library(terra); library(dplyr); library(stringr)
  library(tidyr); library(ggplot2)
})

sf_use_s2(FALSE)   # planar ops in projected CRS below

## ---- 0 . config -----------------------------------------------------
ROOT      <- Sys.getenv("PIPELINE_ROOT", "/data/stubble_burnt")
RUN_TAG   <- "uttar_pradesh_rabi_2026_20260301_to_20260630"
RUN_DIR   <- file.path(ROOT, "data/from_gcs/pipeline", RUN_TAG)

DNBR_DIR  <- file.path(RUN_DIR, "dnbr")
BASE_DIR  <- file.path(RUN_DIR, "baselines")
GFSAD_TIF <- file.path(RUN_DIR, "gfsad", paste0(RUN_TAG, "_gfsad30_20m.tif"))

GPKG_ADMIN <- file.path(ROOT, "gpkg", "uttar_pradesh_admin_with_divisions.gpkg")

## UP42 catalogue exports. Point AOI_DIR at wherever you scp'd the
## AOI_selection folder; the WorldView Legion export is the target but
## the others are read if present for comparison.
AOI_DIR   <- file.path(ROOT, "AOI_selection", "up42_area_over_UP_rabi_2026")
CAT_GPKG  <- file.path(AOI_DIR, "20260804_up42_worldview_legion_acquisitions.gpkg")

OUT_DIR   <- file.path(ROOT, "AOI_selection", "outputs")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

## UP's processing CRS (state_registry.R: utm_crs = EPSG:32644).
## All overlap geometry is reconciled here regardless of native tile zone.
WORK_CRS  <- "EPSG:32644"
AREA_CRS  <- "EPSG:6933"    # equal-area, for any km2 that gets reported

DNBR_BURN_MIN <- 0.10       # must match CFG$dnbr_burn_min for the run

## temporal tolerances
BRACKET_TIGHT_DAYS <- 5     # gold-standard pre/post bracket
BRACKET_OK_DAYS    <- 12    # usable bracket
SCAR_WINDOW_DAYS   <- 21    # VHR still sees a scar this long after burn

## how many scenes to run the (expensive) raster sampling on
N_SAMPLE_SCENES <- 40

## ---- 1 . dNBR tile inventory (zone-agnostic) ------------------------
## MGRS pattern: 2-digit zone + band letter + 2 column/row letters,
## e.g. 43RGP, 44RKQ, 45QXE. The Haryana scripts hardcoded 43R.
MGRS_RX <- "([0-9]{2}[A-Z]{3})"

dnbr_meta <- tibble(
  path = list.files(DNBR_DIR, full.names = TRUE,
                    pattern = "_[0-9]{8}_[0-9]{2}[A-Z]{3}_dnbr\\.tif$")
) |>
  mutate(
    fname    = basename(path),
    obs_date = as.Date(str_match(fname, "_([0-9]{8})_[0-9]{2}[A-Z]{3}_dnbr")[, 2],
                       format = "%Y%m%d"),
    tile     = str_match(fname, paste0("_", MGRS_RX, "_dnbr\\.tif$"))[, 2],
    zone     = substr(tile, 1, 2)
  )

stopifnot(nrow(dnbr_meta) > 0, !anyNA(dnbr_meta$obs_date), !anyNA(dnbr_meta$tile))

tile_obs <- dnbr_meta |>
  group_by(tile, zone) |>
  summarise(n_dates = n(), first_obs = min(obs_date), last_obs = max(obs_date),
            .groups = "drop") |>
  arrange(desc(n_dates))

cat("\n== dNBR tiles ==\n")
cat("  tiles:", nrow(tile_obs), " files:", nrow(dnbr_meta),
    " zones:", paste(sort(unique(tile_obs$zone)), collapse = "/"), "\n")
write.csv(tile_obs, file.path(OUT_DIR, "dnbr_tile_inventory.csv"),
          row.names = FALSE)

## ---- 2 . tile footprints, reprojected to a common CRS ---------------
## Each tile raster is in its NATIVE UTM zone. Building the footprint
## from its own CRS and then transforming is the only correct order;
## assuming a single CRS across tiles is exactly the failure that
## silently dropped five districts from the earlier UP run.
fp_source <- list.files(BASE_DIR, full.names = TRUE,
                        pattern = "_[0-9]{2}[A-Z]{3}_baseline\\.tif$")
if (!length(fp_source)) {
  message("No baseline rasters found - deriving footprints from dNBR tiles.")
  fp_source <- dnbr_meta |> distinct(tile, .keep_all = TRUE) |> pull(path)
}

tile_fp <- do.call(rbind, lapply(fp_source, function(f) {
  r  <- rast(f)
  bb <- st_bbox(c(xmin = xmin(r), ymin = ymin(r),
                  xmax = xmax(r), ymax = ymax(r)),
                crs = st_crs(crs(r)))
  st_sf(tile     = str_match(basename(f), paste0("_", MGRS_RX, "_"))[, 2],
        src_crs  = crs(r, describe = TRUE)$code,
        geometry = st_as_sfc(bb)) |>
    st_transform(WORK_CRS)
})) |>
  distinct(tile, .keep_all = TRUE)

cat("  footprints:", nrow(tile_fp),
    "| native CRS present:", paste(sort(unique(tile_fp$src_crs)), collapse = ", "), "\n")

## ---- 3 . admin layers -----------------------------------------------
districts <- st_read(GPKG_ADMIN, layer = "district_boundary", quiet = TRUE) |>
  st_make_valid() |> st_transform(WORK_CRS)
divisions <- st_read(GPKG_ADMIN, layer = "division_boundary", quiet = TRUE) |>
  st_make_valid() |> st_transform(WORK_CRS)

## ---- 4 . UP42 catalogue ---------------------------------------------
stopifnot(file.exists(CAT_GPKG))
cat_all <- st_read(CAT_GPKG, quiet = TRUE)

cat("\n== UP42 catalogue columns ==\n"); print(names(cat_all))

## Column names vary between UP42 export vintages. Resolve defensively
## rather than assuming the Haryana export's schema.
pick_col <- function(df, candidates, what) {
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) stop("No column for ", what, " in the catalogue. ",
                         "Looked for: ", paste(candidates, collapse = ", "),
                         "\nAvailable: ", paste(names(df), collapse = ", "))
  hit[1]
}
col_id   <- pick_col(cat_all, c("id", "sceneId", "scene_id", "catalogId"), "scene id")
col_date <- pick_col(cat_all, c("acq_date_utc", "acquisitionDate", "acquisition_date",
                                "acq_date", "datetime"), "acquisition date")
col_cc   <- c("cloudCoverage", "cloud_cover", "cloudCover", "cc")
col_cc   <- col_cc[col_cc %in% names(cat_all)][1]

vhr <- cat_all |>
  mutate(scene_id = .data[[col_id]],
         acq_date = as.Date(.data[[col_date]])) |>
  { \(d) if (!is.na(col_cc)) mutate(d, cloud_pct = as.numeric(.data[[col_cc]]))
         else mutate(d, cloud_pct = NA_real_) }() |>
  st_make_valid() |>
  st_transform(WORK_CRS) |>
  select(scene_id, acq_date, cloud_pct)

vhr$scene_km2 <- as.numeric(st_area(st_transform(vhr, AREA_CRS))) / 1e6

cat("\n== scenes ==\n")
cat("  n:", nrow(vhr),
    "| dates:", format(min(vhr$acq_date)), "->", format(max(vhr$acq_date)), "\n")

## ---- 5 . spatial overlap: scene x tile ------------------------------
st_agr(vhr) <- "constant"; st_agr(tile_fp) <- "constant"

ov <- st_intersection(select(vhr, scene_id, acq_date), tile_fp)
ov$tile_overlap_km2 <- as.numeric(st_area(st_transform(ov, AREA_CRS))) / 1e6
ov <- filter(ov, tile_overlap_km2 > 0.01)

## ---- 6 . temporal bracketing per scene x tile -----------------------
obs <- distinct(dnbr_meta, tile, obs_date)

bracket <- ov |>
  st_drop_geometry() |>
  left_join(obs, by = "tile", relationship = "many-to-many") |>
  group_by(scene_id, acq_date, tile, tile_overlap_km2) |>
  summarise(
    n_obs_tile = sum(!is.na(obs_date)),
    pre_date  = if (any(obs_date <= acq_date, na.rm = TRUE))
                  max(obs_date[obs_date <= acq_date], na.rm = TRUE) else as.Date(NA),
    post_date = if (any(obs_date >= acq_date, na.rm = TRUE))
                  min(obs_date[obs_date >= acq_date], na.rm = TRUE) else as.Date(NA),
    .groups = "drop"
  ) |>
  mutate(days_since_pre = as.integer(acq_date - pre_date),
         days_to_post   = as.integer(post_date - acq_date),
         bracket_days   = days_since_pre + days_to_post)

write.csv(bracket, file.path(OUT_DIR, "vhr_tile_bracket.csv"), row.names = FALSE)

## ---- 7 . district / division attribution ----------------------------
dist_ov <- st_intersection(select(vhr, scene_id),
                           select(districts, DISTRICT, DIVISION))
dist_ov$km2 <- as.numeric(st_area(st_transform(dist_ov, AREA_CRS))) / 1e6

scene_admin <- dist_ov |>
  st_drop_geometry() |>
  group_by(scene_id) |>
  summarise(
    n_districts  = n_distinct(DISTRICT),
    n_divisions  = n_distinct(DIVISION),
    top_division = DIVISION[which.max(km2)],
    districts    = paste(sort(unique(DISTRICT)), collapse = "; "),
    land_km2     = sum(km2),
    .groups = "drop"
  )

## ---- 8 . cropland + burn content (sampled on top candidates) --------
## Cheap pre-rank first, so raster sampling only touches plausible scenes.
prelim <- bracket |>
  group_by(scene_id, acq_date) |>
  summarise(overlap_km2      = sum(tile_overlap_km2),
            km2_bracket_tight = sum(tile_overlap_km2[which(bracket_days <= BRACKET_TIGHT_DAYS)]),
            km2_bracket_ok    = sum(tile_overlap_km2[which(bracket_days <= BRACKET_OK_DAYS)]),
            tightest_bracket  = suppressWarnings(min(bracket_days, na.rm = TRUE)),
            tiles             = paste(sort(unique(tile)), collapse = " "),
            .groups = "drop") |>
  mutate(tightest_bracket = ifelse(is.infinite(tightest_bracket), NA_integer_,
                                   tightest_bracket)) |>
  left_join(scene_admin, by = "scene_id") |>
  arrange(desc(km2_bracket_ok), desc(overlap_km2))

cand <- head(prelim$scene_id, N_SAMPLE_SCENES)
cat("\n== sampling rasters for", length(cand), "candidate scenes ==\n")

## 8a. cropland fraction from the run's own GFSAD mask, so the number
## refers to the same denominator the pipeline used.
crop_frac <- tibble(scene_id = character(), cropland_frac = numeric())
if (file.exists(GFSAD_TIF)) {
  gf <- rast(GFSAD_TIF)
  gv <- vect(st_transform(filter(vhr, scene_id %in% cand), crs(gf)))
  cf <- terra::extract(gf[[1]], gv, fun = function(x) mean(x == 1, na.rm = TRUE))
  crop_frac <- tibble(scene_id = gv$scene_id, cropland_frac = round(cf[[2]], 3))
} else {
  message("GFSAD mask not found - skipping cropland fraction.")
}

## 8b. flagged-burn fraction on the bracketing post date.
## This is the commission-error signal: how much of the scene the v6
## product claims as burned. High values are GOOD for validation - they
## are precisely where VHR adjudicates char vs cleared field.
burn_rows <- list()
for (sid in cand) {
  b <- bracket |> filter(scene_id == sid, !is.na(post_date)) |>
       arrange(bracket_days)
  if (!nrow(b)) next
  geom <- filter(vhr, scene_id == sid)
  fr <- c(); wt <- c()
  for (k in seq_len(min(3, nrow(b)))) {          # up to 3 tiles per scene
    f <- dnbr_meta$path[dnbr_meta$tile == b$tile[k] &
                        dnbr_meta$obs_date == b$post_date[k]]
    if (!length(f)) next
    r <- rast(f[1])[[1]]
    g <- vect(st_transform(geom, crs(r)))
    v <- try(terra::extract(r, g, fun = function(x)
               mean(x >= DNBR_BURN_MIN, na.rm = TRUE))[1, 2], silent = TRUE)
    if (inherits(v, "try-error") || is.na(v)) next
    fr <- c(fr, v); wt <- c(wt, b$tile_overlap_km2[k])
  }
  if (length(fr))
    burn_rows[[sid]] <- tibble(scene_id = sid,
                               burn_frac = round(weighted.mean(fr, wt), 4))
}
burn_frac <- if (length(burn_rows)) bind_rows(burn_rows) else
             tibble(scene_id = character(), burn_frac = numeric())

## ---- 9 . scoring -----------------------------------------------------
## Four components, each 0-1, weighted. Deliberately NOT a single
## optimum: the shortlist is spread across divisions in step 10.
##   temporal  - how tightly S2 brackets the VHR date
##   cropland  - burnable land, not city/forest/water
##   diagnostic- flagged-burn content (commission-error adjudication)
##   coverage  - usable overlap area
scored <- prelim |>
  left_join(crop_frac, by = "scene_id") |>
  left_join(burn_frac, by = "scene_id") |>
  mutate(
    s_temporal = case_when(
      is.na(tightest_bracket)                  ~ 0,
      tightest_bracket <= BRACKET_TIGHT_DAYS   ~ 1,
      tightest_bracket <= BRACKET_OK_DAYS      ~ 0.6,
      TRUE                                     ~ 0.2),
    s_cropland = ifelse(is.na(cropland_frac), NA_real_,
                        pmin(cropland_frac / 0.8, 1)),
    ## peak diagnostic value where the scene is neither all-flagged nor
    ## all-clear: a mixed scene yields both commission and omission
    ## evidence in one order.
    s_diag     = ifelse(is.na(burn_frac), NA_real_,
                        1 - abs(burn_frac - 0.4) / 0.6),
    s_coverage = pmin(km2_bracket_ok / quantile(km2_bracket_ok, 0.9,
                                                na.rm = TRUE), 1),
    score = round(
      0.35 * s_temporal +
      0.25 * coalesce(s_cropland, 0) +
      0.25 * coalesce(s_diag, 0) +
      0.15 * s_coverage, 4)
  ) |>
  arrange(desc(score))

write.csv(scored, file.path(OUT_DIR, "vhr_scene_scored.csv"), row.names = FALSE)

cat("\n== top 15 scenes ==\n")
print(scored |>
        select(scene_id, acq_date, top_division, tightest_bracket,
               cropland_frac, burn_frac, km2_bracket_ok, score) |>
        head(15), n = 15, width = Inf)

## ---- 10 . spread the shortlist across divisions ----------------------
## Taking the global top-N clusters wherever WorldView happened to fly.
## Best scene per division first, then fill by score.
PER_DIVISION <- 1
TOTAL_PICK   <- 12

best_per_div <- scored |>
  filter(!is.na(top_division)) |>
  group_by(top_division) |>
  slice_max(score, n = PER_DIVISION, with_ties = FALSE) |>
  ungroup()

fill <- scored |>
  filter(!scene_id %in% best_per_div$scene_id) |>
  slice_max(score, n = max(0, TOTAL_PICK - nrow(best_per_div)), with_ties = FALSE)

selected <- bind_rows(best_per_div, fill) |>
  arrange(desc(score)) |>
  head(TOTAL_PICK)

cat("\n== selected", nrow(selected), "scenes across",
    n_distinct(selected$top_division), "divisions ==\n")
print(selected |> select(scene_id, acq_date, top_division, score,
                         cropland_frac, burn_frac), n = Inf, width = Inf)

sel_sf <- vhr |> filter(scene_id %in% selected$scene_id) |>
  left_join(select(selected, scene_id, score, top_division,
                   cropland_frac, burn_frac, tightest_bracket),
            by = "scene_id")

st_write(sel_sf, file.path(OUT_DIR, "vhr_selected_scenes.gpkg"),
         layer = "selected_scenes", delete_dsn = TRUE, quiet = TRUE)

## ---- 11 . map --------------------------------------------------------
p <- ggplot() +
  geom_sf(data = divisions, fill = NA, colour = "grey70", linewidth = 0.3) +
  geom_sf(data = districts, fill = NA, colour = "grey85", linewidth = 0.15) +
  geom_sf(data = vhr, fill = "grey60", colour = NA, alpha = 0.25) +
  geom_sf(data = sel_sf, aes(fill = score), colour = "#1a237e",
          alpha = 0.75, linewidth = 0.3) +
  scale_fill_viridis_c(option = "C", name = "score") +
  labs(title = "WorldView Legion scenes selected for UP Rabi 2026 validation",
       subtitle = paste0(nrow(sel_sf), " of ", nrow(vhr),
                         " scenes \u00b7 grey = all catalogue footprints"),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11)

ggsave(file.path(OUT_DIR, "vhr_selection_map.png"), p,
       width = 11, height = 8, dpi = 200, bg = "white")

cat("\nDone. Outputs in:", OUT_DIR, "\n")
