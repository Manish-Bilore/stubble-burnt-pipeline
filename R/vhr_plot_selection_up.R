## ===================================================================
## vhr_plot_selection_up.R
## Place 100 m validation plots that STRADDLE the burned/unburned
## boundary, so each plot yields commission-error evidence in a single
## VHR interpretation.
##
## Runs on the GCP VM against /data/stubble_burnt.
##
## INPUTS
##   AOI_selection/outputs/vhr_selected_scenes.gpkg   (from vhr_aoi_selection_up.R)
##   data/from_gcs/pipeline/<RUN_TAG>/dnbr/*.tif      (per-tile, per-date dNBR)
##   data/from_gcs/pipeline/<RUN_TAG>/gfsad/*.tif     (cropland mask)
##
## IMPORTANT - WHICH BURN MASK
##   There are no v7 results yet; the char-confirmation gate is still in
##   design. The burn mask here is therefore v6 dNBR thresholded at
##   BURN_THRESHOLD (0.54), which sits in the Moderate-High severity band
##   rather than the pipeline's 0.10 default. That is a SEVERITY proxy
##   for a char gate, not a char gate: it will still include any
##   non-combustion process that drives a large NBR drop. When v7 lands,
##   point BURN_MASK_MODE at the confirmed-char raster and rerun; nothing
##   else in this script needs to change.
##
## WINDOW SCORING
##   For a fixed 100 m plot on a 20 m grid, a 5x5 focal mean gives the
##   exact class fraction for the window centred on every pixel. That is
##   equivalent to querying a summed-area table at fixed window size, and
##   terra's focal() does it in C++ - so no integral image is built here.
##   If variable plot sizes are ever needed, an integral image becomes
##   the better structure; at one fixed size it is redundant work.
##
## OUTPUTS -> AOI_selection/plot_outputs/
##   plot_candidates_<scene>.tif    per-scene score surface (optional)
##   validation_plots.gpkg          plots + packed order AOIs
##   validation_plots.csv           plot attributes incl. lon/lat
##   up42_order_aois.csv            AOI credit accounting
##   plot_selection_summary.csv     per-scene tallies
## ===================================================================

suppressPackageStartupMessages({
  library(sf); library(terra); library(dplyr); library(stringr); library(tibble)
})

## ---- 0 . config -----------------------------------------------------
ROOT     <- Sys.getenv("PIPELINE_ROOT", "/data/stubble_burnt")
RUN_TAG  <- "uttar_pradesh_rabi_2026_20260301_to_20260630"
RUN_DIR  <- file.path(ROOT, "data/from_gcs/pipeline", RUN_TAG)

DNBR_DIR  <- file.path(RUN_DIR, "dnbr")
GFSAD_TIF <- file.path(RUN_DIR, "gfsad", paste0(RUN_TAG, "_gfsad30_20m.tif"))
SEL_GPKG  <- file.path(ROOT, "AOI_selection/outputs/vhr_selected_scenes.gpkg")
BRACKET   <- file.path(ROOT, "AOI_selection/outputs/vhr_tile_bracket.csv")

OUT_DIR   <- file.path(ROOT, "AOI_selection", "plot_outputs")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

AREA_CRS  <- "EPSG:6933"     # equal-area, for reported km2

## --- burn mask -------------------------------------------------------
## "dnbr_threshold" : v6 dNBR >= BURN_THRESHOLD           (available now)
## "v7_mask"        : a confirmed-char raster from v7     (not yet built)
BURN_MASK_MODE <- "dnbr_threshold"
BURN_THRESHOLD <- 0.54
V7_MASK_TIF    <- NA_character_   # set when v7 exists

## --- plot geometry ---------------------------------------------------
PIX_M     <- 20                    # pipeline native resolution
PLOT_M    <- 100                   # plot edge -> 5 x 5 pixels
KERN      <- PLOT_M / PIX_M
MIN_SEP_M <- 300                   # centre-to-centre thinning distance

## --- window scoring --------------------------------------------------
## Your brief was 0.7 x crop + 0.3 x burn. Taken literally, the burn term
## is maximised by a FULLY burned window - which is the opposite of a
## plot that straddles the boundary. BOUNDARY_MODE = TRUE replaces it
## with a term peaking at 50% burned. Set FALSE for the literal form.
W_CROP        <- 0.7
W_BURN        <- 0.3
BOUNDARY_MODE <- TRUE
MIN_CROP_FRAC <- 0.60              # reject windows below this
MIN_BURN_FRAC <- 0.20              # window must contain some of each
MAX_BURN_FRAC <- 0.80

N_PLOTS_PER_SCENE <- 25

## --- UP42 order economics -------------------------------------------
## NOTE: the credits/km2 rate for WorldView Legion is NOT the phr (1000)
## or pneo (1800) rate carried in vhr_validation_design.R. Confirm the
## current rate and minimum AOI in your UP42 console before ordering -
## the figures below are placeholders and will misstate cost if wrong.
CREDITS_PER_KM2 <- NA_real_
MIN_AOI_KM2     <- 5
ORDER_PAD_M     <- 500

## ---- 1 . inputs ------------------------------------------------------
stopifnot(file.exists(SEL_GPKG), file.exists(BRACKET))

sel <- st_read(SEL_GPKG, quiet = TRUE)
brk <- read.csv(BRACKET, stringsAsFactors = FALSE) |>
  mutate(acq_date  = as.Date(acq_date),
         post_date = as.Date(post_date),
         pre_date  = as.Date(pre_date))

dnbr_meta <- tibble(
  path = list.files(DNBR_DIR, full.names = TRUE,
                    pattern = "_[0-9]{8}_[0-9]{2}[A-Z]{3}_dnbr\\.tif$")
) |>
  mutate(fname    = basename(path),
         obs_date = as.Date(str_match(fname, "_([0-9]{8})_[0-9]{2}[A-Z]{3}_dnbr")[, 2], "%Y%m%d"),
         tile     = str_match(fname, "_([0-9]{2}[A-Z]{3})_dnbr\\.tif$")[, 2])

gf <- if (file.exists(GFSAD_TIF)) rast(GFSAD_TIF) else
        stop("GFSAD mask not found: ", GFSAD_TIF)

cat("== inputs ==\n")
cat("  scenes:", nrow(sel), " dNBR files:", nrow(dnbr_meta), "\n")
cat("  burn mask:", BURN_MASK_MODE,
    if (BURN_MASK_MODE == "dnbr_threshold")
      paste0(" (dNBR >= ", BURN_THRESHOLD, ")") else "", "\n")
cat("  scoring:", W_CROP, "x crop +", W_BURN, "x",
    if (BOUNDARY_MODE) "boundary" else "burn", "\n\n")

## ---- 2 . per-scene plot placement -----------------------------------
plots_all <- list(); aois_all <- list(); summary_rows <- list()

for (i in seq_len(nrow(sel))) {

  sid <- sel$scene_id[i]
  geom <- sel[i, ]
  P    <- as.Date(sel$acq_date[i])
  cat("--", sid, "|", format(P), "|", sel$top_division[i], "\n")

  ## 2a. bracketing dNBR tiles for this scene, tightest first
  b <- brk |>
    filter(scene_id == sid, !is.na(post_date)) |>
    arrange(bracket_days)
  if (!nrow(b)) { cat("   no bracketing obs - skipped\n"); next }

  ## 2b. assemble the burn mask over the scene footprint
  burn_parts <- list()
  for (k in seq_len(nrow(b))) {
    f <- dnbr_meta$path[dnbr_meta$tile == b$tile[k] &
                        dnbr_meta$obs_date == b$post_date[k]]
    if (!length(f)) next
    r <- rast(f[1])[[1]]
    g <- vect(st_transform(geom, crs(r)))
    rc <- try(crop(r, g), silent = TRUE)
    if (inherits(rc, "try-error") || ncell(rc) == 0) next
    rc <- mask(rc, g)
    burn_parts[[length(burn_parts) + 1]] <- rc
  }
  if (!length(burn_parts)) { cat("   no dNBR coverage - skipped\n"); next }

  dn <- if (length(burn_parts) == 1) burn_parts[[1]] else {
    ## tiles may sit in different UTM zones - align before merging
    ref <- burn_parts[[1]]
    aligned <- lapply(burn_parts, function(x)
      if (same.crs(x, ref)) x else project(x, ref, method = "bilinear"))
    do.call(merge, aligned)
  }

  burn <- if (BURN_MASK_MODE == "v7_mask" && !is.na(V7_MASK_TIF)) {
    v7 <- rast(V7_MASK_TIF)
    resample(crop(v7, dn), dn, method = "near")
  } else {
    ifel(is.na(dn), NA, dn >= BURN_THRESHOLD)
  }

  ## 2c. cropland on the same grid
  crops <- resample(crop(gf[[1]], project(as.polygons(ext(dn), crs = crs(dn)),
                                          crs(gf))),
                    dn, method = "near")
  crops <- ifel(is.na(crops), 0, crops == 1)

  ## 2d. window fractions via focal mean (= summed-area query at fixed size)
  w <- matrix(1, KERN, KERN)
  crop_frac <- focal(crops * 1, w, fun = "mean", na.policy = "omit",
                     na.rm = TRUE, expand = FALSE)
  burn_frac <- focal(ifel(is.na(burn), 0, burn * 1), w, fun = "mean",
                     na.rm = TRUE, expand = FALSE)
  ## fraction of the window with any valid dNBR - a window that is mostly
  ## cloud-masked must not score well merely because it looks unburned
  valid_frac <- focal(ifel(is.na(dn), 0, 1), w, fun = "mean",
                      na.rm = TRUE, expand = FALSE)

  ## 2e. score
  s_burn <- if (BOUNDARY_MODE) 1 - 2 * abs(burn_frac - 0.5) else burn_frac
  score  <- W_CROP * crop_frac + W_BURN * s_burn

  elig <- (crop_frac >= MIN_CROP_FRAC) &
          (burn_frac >= MIN_BURN_FRAC) & (burn_frac <= MAX_BURN_FRAC) &
          (valid_frac >= 0.90)
  score <- mask(score, ifel(elig, 1, NA))

  n_elig <- global(ifel(elig, 1, NA), "sum", na.rm = TRUE)[[1]]
  if (is.na(n_elig) || n_elig < 1) {
    cat("   no eligible boundary windows - skipped\n")
    summary_rows[[sid]] <- tibble(scene_id = sid, acq_date = P,
                                  n_eligible = 0, n_plots = 0)
    next
  }
  cat("   eligible windows:", n_elig, "\n")

  ## 2f. greedy thinning: best score first, then exclude a MIN_SEP_M disc
  cand <- as.data.frame(score, xy = TRUE, na.rm = TRUE)
  names(cand)[3] <- "score"
  cand <- cand[order(-cand$score), ]
  cand <- head(cand, 20000)          # cap the search

  keep <- integer(0)
  for (r_i in seq_len(nrow(cand))) {
    if (!length(keep)) { keep <- r_i; next }
    d <- sqrt((cand$x[r_i] - cand$x[keep])^2 + (cand$y[r_i] - cand$y[keep])^2)
    if (min(d) >= MIN_SEP_M) keep <- c(keep, r_i)
    if (length(keep) >= N_PLOTS_PER_SCENE) break
  }
  sel_pts <- cand[keep, ]
  cat("   plots placed:", nrow(sel_pts), "\n")

  ## 2g. plot polygons + attributes
  ctr <- st_as_sf(sel_pts, coords = c("x", "y"), crs = crs(dn))
  att <- terra::extract(c(crop_frac, burn_frac, valid_frac),
                        vect(ctr))[, -1, drop = FALSE]
  names(att) <- c("crop_frac", "burn_frac", "valid_frac")

  pw <- st_buffer(ctr, PLOT_M / 2, endCapStyle = "SQUARE")
  pw$scene_id  <- sid
  pw$acq_date  <- P
  pw$division  <- sel$top_division[i]
  pw$post_date <- b$post_date[1]
  pw$pre_date  <- b$pre_date[1]
  pw$bracket_days <- b$bracket_days[1]
  pw <- cbind(pw, round(att, 4))
  pw$stratum <- "boundary"

  ll <- st_coordinates(st_transform(st_centroid(pw), 4326))
  pw$lon <- round(ll[, 1], 6); pw$lat <- round(ll[, 2], 6)
  pw <- st_transform(pw, 4326)
  plots_all[[sid]] <- pw

  ## 2h. pack order AOIs around the plots
  pwu   <- st_transform(pw, crs(dn))
  blob  <- st_union(st_buffer(st_geometry(pwu), ORDER_PAD_M))
  parts <- st_cast(blob, "POLYGON")
  boxes <- do.call(c, lapply(seq_along(parts),
                             function(j) st_as_sfc(st_bbox(parts[j]))))
  ao <- st_sf(scene_id = sid, acq_date = P, geometry = boxes)
  st_crs(ao) <- crs(dn)
  ao$aoi_km2    <- round(as.numeric(st_area(st_transform(ao, AREA_CRS))) / 1e6, 2)
  ao$billed_km2 <- pmax(ao$aoi_km2, MIN_AOI_KM2)
  aois_all[[sid]] <- st_transform(ao, 4326)

  summary_rows[[sid]] <- tibble(
    scene_id = sid, acq_date = P, division = sel$top_division[i],
    n_eligible = n_elig, n_plots = nrow(pw),
    mean_crop = round(mean(att$crop_frac), 3),
    mean_burn = round(mean(att$burn_frac), 3),
    aoi_km2   = sum(ao$aoi_km2), billed_km2 = sum(ao$billed_km2))
}

## ---- 3 . outputs -----------------------------------------------------
smry <- bind_rows(summary_rows)
cat("\n== per-scene summary ==\n"); print(smry, n = Inf, width = Inf)
write.csv(smry, file.path(OUT_DIR, "plot_selection_summary.csv"),
          row.names = FALSE)

if (length(plots_all)) {
  plots <- do.call(rbind, plots_all)
  plots$plot_id <- sprintf("%s_%03d", format(plots$acq_date, "%Y%m%d"),
                           seq_len(nrow(plots)))
  st_write(plots, file.path(OUT_DIR, "validation_plots.gpkg"),
           layer = "plots", delete_dsn = TRUE, quiet = TRUE)
  write.csv(st_drop_geometry(plots),
            file.path(OUT_DIR, "validation_plots.csv"), row.names = FALSE)
  cat("\nplots written:", nrow(plots), "across",
      n_distinct(plots$scene_id), "scenes\n")
  cat("  mean burn_frac:", round(mean(plots$burn_frac), 3),
      "(0.5 = perfectly straddling)\n")
  cat("  mean crop_frac:", round(mean(plots$crop_frac), 3), "\n")
}

if (length(aois_all)) {
  aois <- do.call(rbind, aois_all)
  st_write(aois, file.path(OUT_DIR, "validation_plots.gpkg"),
           layer = "order_aois", append = TRUE, quiet = TRUE)
  write.csv(st_drop_geometry(aois),
            file.path(OUT_DIR, "up42_order_aois.csv"), row.names = FALSE)

  cat("\n== UP42 order accounting ==\n")
  cat("  AOIs:", nrow(aois), "|", sum(aois$aoi_km2), "km2 drawn,",
      sum(aois$billed_km2), "km2 billed\n")
  n_small <- sum(aois$aoi_km2 < MIN_AOI_KM2)
  if (n_small > 0)
    cat("  NOTE:", n_small, "AOIs below the", MIN_AOI_KM2,
        "km2 minimum - billed at the floor. Raise ORDER_PAD_M or",
        "MIN_SEP_M to merge them.\n")
  if (is.na(CREDITS_PER_KM2)) {
    cat("  Credits NOT computed: set CREDITS_PER_KM2 to the WorldView\n",
        "  Legion rate from your UP42 console. Do not assume the phr\n",
        "  (1000) or pneo (1800) rate applies.\n")
  } else {
    cat("  Credits:", round(sum(aois$billed_km2) * CREDITS_PER_KM2), "\n")
  }
}

cat("\nDone. Outputs in:", OUT_DIR, "\n")
