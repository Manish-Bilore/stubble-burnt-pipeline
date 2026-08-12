## ===================================================================
## vhr_aoi_selection_burnfirst_up.R
## BURN-FIRST AOI selection for UP Rabi 2026.
##
## Companion to vhr_aoi_selection_up.R (scene-first), which starts from
## the UP42 catalogue and scores every scene. This one inverts the
## search: start from the burnt-area map, find the densest flagged
## burn, then ask which WorldView Legion scenes cover it.
##
##   scene-first  : "of the scenes on offer, which are most useful?"
##   burn-first   : "where is the most burnt area, and can we image it?"
##
## READ THIS BEFORE ORDERING
##   "Most burnt area" here means "most area FLAGGED burnt by v6". The
##   v6 product over-detects: it fires on dry harvested stubble as well
##   as char, which is why UP Rabi 2026 returns implausible district
##   totals. So this script targets the areas where the pipeline is most
##   confident, which is also where a systematic commission error would
##   do the most damage to a published figure.
##
##   That makes the output well suited to:
##     - quantifying commission error where it matters most by area
##     - evidence for the UP consultation ("we flag X, VHR shows Y")
##   and NOT suited to:
##     - a representative sample of actual burning in UP
##     - omission-error estimation (no unflagged strata are sought)
##
##   BURN_THRESHOLD is set to 0.54 (Moderate-High severity), not the
##   pipeline's 0.10 default, to bias toward severe drops that are more
##   plausibly char. It is a severity proxy, not a char gate. When v7
##   lands, point BURN_SOURCE at the confirmed-char product.
##
## OUTPUTS -> AOI_selection/burnfirst_outputs/
##   district_burn_rank.csv        districts ranked by flagged burn
##   hotspot_grid.gpkg             burn-density grid cells (hotspots)
##   scene_burn_capture.csv        scenes ranked by burnt area captured
##   burnfirst_selected.gpkg       selected scenes + packed order AOIs
##   burnfirst_map.png             hotspots vs scenes vs districts
## ===================================================================

suppressPackageStartupMessages({
  library(sf); library(terra); library(dplyr); library(stringr)
  library(tibble); library(ggplot2)
})

sf_use_s2(FALSE)

## ---- 0 . config -----------------------------------------------------
ROOT     <- Sys.getenv("PIPELINE_ROOT", "/data/stubble_burnt")
RUN_TAG  <- "uttar_pradesh_rabi_2026_20260301_to_20260630"
RUN_DIR  <- file.path(ROOT, "data/from_gcs/pipeline", RUN_TAG)

DNBR_DIR   <- file.path(RUN_DIR, "dnbr")
GFSAD_TIF  <- file.path(RUN_DIR, "gfsad", paste0(RUN_TAG, "_gfsad30_20m.tif"))
GEOTIFF_DIR<- file.path(ROOT, "data/outputs/geotiff")     # cumulative products
CSV_DIR    <- file.path(ROOT, "data/outputs/csv")
GPKG_ADMIN <- file.path(ROOT, "gpkg", "uttar_pradesh_admin_with_divisions.gpkg")

AOI_DIR  <- file.path(ROOT, "AOI_selection", "up42_area_over_UP_rabi_2026")
CAT_GPKG <- file.path(AOI_DIR, "20260804_up42_worldview_legion_acquisitions.gpkg")

OUT_DIR  <- file.path(ROOT, "AOI_selection", "burnfirst_outputs")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

WORK_CRS <- "EPSG:32644"
AREA_CRS <- "EPSG:6933"

## --- burn definition -------------------------------------------------
## "max_dnbr"  : per-district max_dnbr cumulative rasters (fast, whole season)
## "per_date"  : per-tile per-date dNBR (slower, lets you window by date)
BURN_SOURCE    <- "max_dnbr"
BURN_THRESHOLD <- 0.54

## --- temporal validity ----------------------------------------------
## A scene can only evidence a burn it was acquired AFTER. Scenes are
## matched per hotspot cell against that cell's district first/last
## detection dates, then allowed a scar window afterwards. Without this,
## the ranking happily selects acquisitions that predate the fires by
## weeks - it scores burnt AREA and knows nothing about time.
ENFORCE_TEMPORAL <- TRUE
SCAR_WINDOW_DAYS <- 21    # scar still interpretable this long after burn
LEAD_TOLERANCE_D <- 0     # days a scene may predate first_detection

## --- hotspot grid ----------------------------------------------------
HOTSPOT_CELL_M <- 5000        # 5 km cells; burn density computed per cell
N_HOTSPOTS     <- 60          # carry this many top cells forward
TOP_DISTRICTS  <- 25          # rank districts, then only raster these

## --- selection -------------------------------------------------------
N_SCENES        <- 12
MAX_PER_DIVISION<- 3          # loose cap so one division cannot take all
ORDER_BLOCK_KM2 <- 25         # size of the AOI block cut from each scene
MIN_AOI_KM2     <- 5
CREDITS_PER_KM2 <- NA_real_   # confirm WorldView Legion rate in UP42 console

## ---- 1 . district ranking -------------------------------------------
summ_csv <- list.files(CSV_DIR, pattern = "district_summary\\.csv$",
                       full.names = TRUE)
stopifnot(length(summ_csv) >= 1)
ds <- read.csv(summ_csv[1], stringsAsFactors = FALSE)
names(ds)[1:2] <- c("DISTRICT", "burn_area_ha")
ds$DISTRICT <- toupper(gsub('"', "", ds$DISTRICT))

## Detection dates drive the temporal filter. Resolve by name where the
## summary carries them, rather than assuming column positions.
fd_col <- intersect(c("first_detection", "first_burn", "first_obs"), names(ds))
## "last_first_detection" is the LATEST first-burn date across pixels,
## i.e. the end of the burn-onset period - the right upper bound here.
ld_col <- intersect(c("last_first_detection", "last_detection",
                      "last_burn", "last_obs"), names(ds))
if (length(fd_col)) ds$first_detection <- as.Date(ds[[fd_col[1]]])
if (length(ld_col)) ds$last_detection  <- as.Date(ds[[ld_col[1]]])
if (!length(fd_col)) {
  ds$first_detection <- as.Date(NA); ds$last_detection <- as.Date(NA)
  warning("No first_detection column found - temporal filter disabled.")
  ENFORCE_TEMPORAL <- FALSE
}

districts <- st_read(GPKG_ADMIN, layer = "district_boundary", quiet = TRUE) |>
  st_make_valid() |> st_transform(WORK_CRS)
divisions <- st_read(GPKG_ADMIN, layer = "division_boundary", quiet = TRUE) |>
  st_make_valid() |> st_transform(WORK_CRS)

dist_rank <- districts |>
  st_drop_geometry() |>
  select(DISTRICT, DIVISION, AREA_KM2) |>
  left_join(select(ds, DISTRICT, burn_area_ha,
                   first_detection, last_detection), by = "DISTRICT") |>
  mutate(burn_area_ha = coalesce(burn_area_ha, 0),
         burn_km2     = burn_area_ha / 100,
         burn_frac    = round(burn_km2 / AREA_KM2, 4)) |>
  arrange(desc(burn_area_ha))

cat("== districts by flagged burn area ==\n")
print(as_tibble(head(dist_rank, 15)), width = Inf)
write.csv(dist_rank, file.path(OUT_DIR, "district_burn_rank.csv"),
          row.names = FALSE)

## Sanity flag: a district cannot plausibly be majority-burnt. Report it
## rather than silently proceeding, because it changes interpretation.
n_implausible <- sum(dist_rank$burn_frac > 0.5, na.rm = TRUE)
if (n_implausible > 0)
  cat("\n  WARNING:", n_implausible, "district(s) flagged >50% burnt.",
      "These are commission-error dominated; treat rankings as\n",
      "  'where the detector fires most', not 'where burning is worst'.\n")

top_dist <- head(dist_rank$DISTRICT, TOP_DISTRICTS)

## ---- 2 . burn-density hotspot grid ----------------------------------
## Rasterising per-district cumulative products onto a coarse grid is far
## cheaper than re-reading the 633 per-date tiles, and the max_dnbr
## product already encodes the season's peak severity per pixel.
safe_name <- function(x) gsub("[^A-Za-z0-9_]", "_", toupper(x))

grid <- st_make_grid(districts, cellsize = HOTSPOT_CELL_M, square = TRUE) |>
  st_as_sf() |>
  rename(geometry = x)
grid$cell_id <- seq_len(nrow(grid))
grid <- grid[lengths(st_intersects(grid, districts)) > 0, ]
cat("\n== hotspot grid ==\n  cells:", nrow(grid),
    "at", HOTSPOT_CELL_M / 1000, "km\n")

burn_km2_by_cell <- setNames(rep(0, nrow(grid)), grid$cell_id)

if (BURN_SOURCE == "max_dnbr") {
  found <- 0
  for (d in top_dist) {
    f <- file.path(GEOTIFF_DIR,
                   sprintf("%s_%s_max_dnbr.tif", RUN_TAG, safe_name(d)))
    if (!file.exists(f)) next
    found <- found + 1
    r  <- rast(f)[[1]]
    bm <- ifel(is.na(r), NA, r >= BURN_THRESHOLD)
    ## cell-wise burnt area, in the raster's own CRS
    gsub_cells <- st_transform(grid, crs(bm))
    hit <- which(lengths(st_intersects(gsub_cells,
                    st_as_sfc(st_bbox(bm)))) > 0)
    if (!length(hit)) next
    ex <- terra::extract(bm, vect(gsub_cells[hit, ]),
                         fun = sum, na.rm = TRUE)
    px_area_km2 <- prod(res(bm)) / 1e6
    add <- coalesce(ex[[2]], 0) * px_area_km2
    ids <- as.character(gsub_cells$cell_id[hit])
    burn_km2_by_cell[ids] <- burn_km2_by_cell[ids] + add
  }
  cat("  districts rasterised:", found, "of", length(top_dist), "\n")
  if (found == 0)
    stop("No max_dnbr rasters found in ", GEOTIFF_DIR,
         ". Run step 03, or set BURN_SOURCE = 'per_date'.")
} else {
  stop("BURN_SOURCE = 'per_date' not implemented in this version; ",
       "use max_dnbr, or extend here if date-windowed hotspots are needed.")
}

## Each cell inherits the detection window of the district it sits in,
## so the temporal test is local rather than state-wide.
cell_ctr <- st_centroid(grid)
cd <- st_join(cell_ctr, select(districts, DISTRICT), join = st_within)
grid$DISTRICT <- cd$DISTRICT
grid <- grid |>
  left_join(select(dist_rank, DISTRICT, first_detection, last_detection),
            by = "DISTRICT")

grid$burn_km2  <- as.numeric(burn_km2_by_cell[as.character(grid$cell_id)])
grid$cell_km2  <- as.numeric(st_area(st_transform(grid, AREA_CRS))) / 1e6
grid$burn_frac <- round(grid$burn_km2 / grid$cell_km2, 4)

hot <- grid |> arrange(desc(burn_km2)) |> head(N_HOTSPOTS)
cat("  top cell burnt area:", round(max(grid$burn_km2), 1), "km2",
    "| median of top", N_HOTSPOTS, ":", round(median(hot$burn_km2), 1), "km2\n")

st_write(hot, file.path(OUT_DIR, "hotspot_grid.gpkg"),
         layer = "hotspots", delete_dsn = TRUE, quiet = TRUE)

## ---- 3 . which scenes cover the hotspots? ---------------------------
cat_all <- st_read(CAT_GPKG, quiet = TRUE)
vhr <- cat_all |>
  mutate(scene_id = id, acq_date = as.Date(acq_date_utc),
         cloud_pct = suppressWarnings(as.numeric(cloud_coverage))) |>
  st_make_valid() |> st_transform(WORK_CRS) |>
  select(scene_id, acq_date, cloud_pct)

st_agr(vhr) <- "constant"; st_agr(hot) <- "constant"

## burnt area each scene captures = sum over intersected hotspot cells,
## pro-rated by the intersected fraction of each cell
ov <- st_intersection(select(vhr, scene_id, acq_date),
                      select(hot, cell_id, burn_km2, cell_km2,
                             DISTRICT, first_detection, last_detection))
ov$piece_km2 <- as.numeric(st_area(st_transform(ov, AREA_CRS))) / 1e6
ov$burn_captured_km2 <- ov$burn_km2 * (ov$piece_km2 / ov$cell_km2)

## ---- 3b . temporal validity -----------------------------------------
ov$days_after_first <- as.integer(ov$acq_date - ov$first_detection)
ov$days_after_last  <- as.integer(ov$acq_date - ov$last_detection)
ov$temporally_valid <- !is.na(ov$first_detection) &
  ov$days_after_first >= -LEAD_TOLERANCE_D &
  ov$days_after_last  <= SCAR_WINDOW_DAYS

if (ENFORCE_TEMPORAL) {
  n_before <- nrow(ov)
  km2_before <- sum(ov$burn_captured_km2, na.rm = TRUE)
  dropped <- ov |> st_drop_geometry() |> filter(!temporally_valid)
  ov <- filter(ov, temporally_valid)
  cat("\n== temporal filter ==\n")
  cat("  scene x cell pairs:", n_before, "->", nrow(ov),
      "| flagged burn:", round(km2_before, 1), "->",
      round(sum(ov$burn_captured_km2, na.rm = TRUE), 1), "km2\n")
  if (nrow(dropped)) {
    pre  <- sum(dropped$days_after_first < 0, na.rm = TRUE)
    post <- sum(dropped$days_after_last > SCAR_WINDOW_DAYS, na.rm = TRUE)
    cat("  dropped:", pre, "acquired BEFORE first detection |",
        post, "more than", SCAR_WINDOW_DAYS, "days after last\n")
  }
  if (!nrow(ov))
    stop("No scene intersects a hotspot within its burn window. ",
         "Widen SCAR_WINDOW_DAYS / N_HOTSPOTS, or set ENFORCE_TEMPORAL = FALSE ",
         "to reproduce the untimed ranking.")
}

scene_capture <- ov |>
  st_drop_geometry() |>
  group_by(scene_id, acq_date) |>
  summarise(n_hotspot_cells = n_distinct(cell_id),
            burn_captured_km2 = sum(burn_captured_km2),
            med_days_after_first = median(days_after_first, na.rm = TRUE),
            med_days_after_last  = median(days_after_last, na.rm = TRUE),
            .groups = "drop") |>
  arrange(desc(burn_captured_km2))

## attribute to division for the spread cap
sc_div <- st_intersection(select(vhr, scene_id),
                          select(districts, DISTRICT, DIVISION))
sc_div$km2 <- as.numeric(st_area(st_transform(sc_div, AREA_CRS))) / 1e6
sc_div <- sc_div |> st_drop_geometry() |>
  group_by(scene_id) |>
  summarise(top_division = DIVISION[which.max(km2)],
            districts = paste(sort(unique(DISTRICT)), collapse = "; "),
            .groups = "drop")

scene_capture <- scene_capture |>
  left_join(sc_div, by = "scene_id") |>
  arrange(desc(burn_captured_km2))

cat("\n== scenes by burnt area captured ==\n")
print(as_tibble(head(scene_capture, 15)), width = Inf)
write.csv(scene_capture, file.path(OUT_DIR, "scene_burn_capture.csv"),
          row.names = FALSE)

## ---- 4 . select, with a loose division cap --------------------------
sel <- scene_capture |>
  group_by(top_division) |>
  mutate(rank_in_div = row_number()) |>
  ungroup() |>
  filter(rank_in_div <= MAX_PER_DIVISION) |>
  arrange(desc(burn_captured_km2)) |>
  head(N_SCENES)

cat("\n== selected", nrow(sel), "scenes |",
    n_distinct(sel$top_division), "divisions |",
    round(sum(sel$burn_captured_km2), 1), "km2 flagged burn captured ==\n")
print(select(sel, scene_id, acq_date, top_division, n_hotspot_cells,
             burn_captured_km2, med_days_after_first, med_days_after_last),
      n = Inf, width = Inf)

## ---- 5 . cut an orderable block from the densest part of each scene --
## The full WorldView strip is far larger than anyone would order, so
## take the ORDER_BLOCK_KM2 block centred on the scene's own burn
## centroid (hotspot-weighted), clipped to the footprint.
blocks <- list()
side_m <- sqrt(ORDER_BLOCK_KM2) * 1000

for (i in seq_len(nrow(sel))) {
  sid <- sel$scene_id[i]
  pieces <- ov |> filter(scene_id == sid)
  if (!nrow(pieces)) next
  ctr <- st_coordinates(st_centroid(
           st_union(st_geometry(pieces[which.max(pieces$burn_captured_km2), ]))))
  bb <- st_as_sfc(st_bbox(c(xmin = ctr[1] - side_m / 2,
                            xmax = ctr[1] + side_m / 2,
                            ymin = ctr[2] - side_m / 2,
                            ymax = ctr[2] + side_m / 2),
                          crs = st_crs(vhr)))
  blk <- st_intersection(bb, st_geometry(filter(vhr, scene_id == sid)))
  if (!length(blk)) next
  b <- st_sf(scene_id = sid, acq_date = sel$acq_date[i],
             top_division = sel$top_division[i],
             burn_captured_km2 = round(sel$burn_captured_km2[i], 2),
             geometry = st_sfc(st_union(blk), crs = st_crs(vhr)))
  b$aoi_km2    <- round(as.numeric(st_area(st_transform(b, AREA_CRS))) / 1e6, 2)
  b$billed_km2 <- pmax(b$aoi_km2, MIN_AOI_KM2)
  blocks[[sid]] <- b
}

aois <- do.call(rbind, blocks)

cat("\n== order AOIs ==\n")
print(as_tibble(st_drop_geometry(aois)), width = Inf)
cat("\n  drawn:", round(sum(aois$aoi_km2), 1), "km2 |",
    "billed:", round(sum(aois$billed_km2), 1), "km2\n")
if (is.na(CREDITS_PER_KM2)) {
  cat("  Credits NOT computed - set CREDITS_PER_KM2 from the UP42 console.\n")
} else {
  cat("  Credits:", round(sum(aois$billed_km2) * CREDITS_PER_KM2), "\n")
}

sel_sf <- vhr |> filter(scene_id %in% sel$scene_id) |>
  left_join(select(sel, scene_id, top_division, burn_captured_km2,
                   n_hotspot_cells), by = "scene_id")

st_write(sel_sf, file.path(OUT_DIR, "burnfirst_selected.gpkg"),
         layer = "selected_scenes", delete_dsn = TRUE, quiet = TRUE)
st_write(aois, file.path(OUT_DIR, "burnfirst_selected.gpkg"),
         layer = "order_aois", append = TRUE, quiet = TRUE)

## ---- 6 . map ---------------------------------------------------------
p <- ggplot() +
  geom_sf(data = divisions, fill = NA, colour = "grey70", linewidth = 0.3) +
  geom_sf(data = districts, fill = NA, colour = "grey88", linewidth = 0.15) +
  geom_sf(data = hot, aes(fill = burn_km2), colour = NA, alpha = 0.8) +
  geom_sf(data = sel_sf, fill = NA, colour = "#1a237e", linewidth = 0.4) +
  geom_sf(data = aois, fill = "#e53935", colour = "#b71c1c", alpha = 0.8) +
  scale_fill_viridis_c(option = "B", name = "flagged burn\nper 5 km cell (km2)") +
  labs(title = "Burn-first AOI selection - UP Rabi 2026",
       subtitle = paste0("top ", N_HOTSPOTS, " hotspot cells \u00b7 ",
                         nrow(sel_sf), " scenes \u00b7 ",
                         nrow(aois), " order blocks (red)\n",
                         "flagged burn = v6 dNBR >= ", BURN_THRESHOLD,
                         "; commission error not yet excluded"),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11)

ggsave(file.path(OUT_DIR, "burnfirst_map.png"), p,
       width = 11, height = 8, dpi = 200, bg = "white")

cat("\nDone. Outputs in:", OUT_DIR, "\n")
