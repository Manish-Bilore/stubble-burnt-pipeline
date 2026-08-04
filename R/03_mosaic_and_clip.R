# =============================================================================
# 03_mosaic_and_clip.R
# For each acquisition date, mosaic all MGRS dNBR tiles into a seamless state
# raster, then clip to each district polygon in parallel using furrr.
# Also produces per-district cumulative products (first-burn DOY, max dNBR,
# cumulative count).
# =============================================================================
# v5 changes vs v4:
#   - Layer name now reads from CFG$gpkg_lyr_dist instead of hardcoded
#   - Removed st_set_crs(4326) → st_transform(4326) pattern, which would
#     silently corrupt geometry if the GPKG were not actually in WGS84.
#     Now uses st_transform(4326) directly, trusting the file's stored CRS.
#   - dist_col now reads from CFG$dist_col (set by state_registry) instead
#     of being auto-discovered each time.
# =============================================================================
# Outputs per district:
#   data/outputs/geotiff/<run_tag>_<DISTRICT>_<YYYYMMDD>_dnbr.tif
#   data/outputs/geotiff/<run_tag>_<DISTRICT>_cumulative_count.tif
#   data/outputs/geotiff/<run_tag>_<DISTRICT>_first_burn_doy.tif
#   data/outputs/geotiff/<run_tag>_<DISTRICT>_max_dnbr.tif
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(lubridate)
  library(glue)
  library(fs)
  library(logger)
  library(future)
  library(furrr)
})

# ── Mosaic helpers ────────────────────────────────────────────────────────────

#' Mosaic all tile dNBR GeoTIFFs for a single date into one SpatRaster.
mosaic_date_tiles <- function(dnbr_dir, date_str, run_tag,
                              target_crs = CFG$target_crs) {
  files <- list.files(dnbr_dir,
    pattern = glue("^{run_tag}_{date_str}_[A-Z0-9]+_dnbr\\.tif$"),
    full.names = TRUE)
  if (length(files) == 0) return(NULL)

  # Tiles are written in their native MGRS UTM zone (43N/44N/45N for UP).
  # terra::mosaic() requires identical CRS - mixed input silently drops
  # tiles outside the first raster's zone, yielding valid_px = 0 on clip.
  # Reproject off-zone tiles onto the grid of an in-zone reference tile, so
  # CRS, resolution AND origin all match. project(r, crs) alone picks its own
  # output resolution, which then fails mosaic()'s resolution check.
  crs_ok <- vapply(files, function(p) same.crs(rast(p), target_crs), logical(1))
  if (!any(crs_ok)) {
    ref <- project(rast(files[1]), target_crs, method = "bilinear")
    res(ref) <- CFG$native_res_m
  } else {
    ref <- rast(files[which(crs_ok)[1]])
  }

  rlist <- lapply(files, function(p) {
    r <- rast(p)
    if (!same.crs(r, target_crs)) {
      tmpl <- project(rast(r), target_crs)      # empty grid, correct extent
      res(tmpl) <- res(ref)
      origin(tmpl) <- origin(ref)
      r <- project(r, tmpl, method = "bilinear")
    }
    r
  })
  if (length(rlist) == 1) return(rlist[[1]])
  do.call(mosaic, c(rlist, list(fun = "mean")))
}

# ── Per-district clip worker ──────────────────────────────────────────────────

#' Worker: clip mosaic to district, reproject to target UTM, write GeoTIFF.
clip_district <- function(mosaic_path, dist_name, dist_sf,
                           out_dir, run_tag, target_crs) {
  safe    <- gsub("[^A-Za-z0-9_]", "_", toupper(dist_name))
  date_str <- regmatches(basename(mosaic_path),
                          regexpr("[0-9]{8}(?=_mosaic)", basename(mosaic_path), perl=TRUE))
  out_path <- file.path(out_dir, glue("{run_tag}_{safe}_{date_str}_dnbr.tif"))

  if (file.exists(out_path)) return(out_path)

  mosaic_r  <- rast(mosaic_path)
  dist_vect <- vect(dist_sf)

  # If mosaic is in WGS84 (from streamed tiles), reproject to target UTM
  if (!same.crs(mosaic_r, target_crs)) {
    mosaic_r <- project(mosaic_r, target_crs, method = "bilinear",
                        threads = TRUE)
  }
  dist_vect_utm <- project(dist_vect, target_crs)

  clipped <- crop(mosaic_r, dist_vect_utm, snap = "out")
  clipped <- mask(clipped, dist_vect_utm)

  # Skip districts with no burned pixels
  if (all(is.na(values(clipped[[1]])))) return(NULL)

  writeRaster(clipped, out_path,
              datatype = "FLT4S",
              gdal = c("COMPRESS=DEFLATE", "TILED=YES",
                       "BLOCKXSIZE=256", "BLOCKYSIZE=256"),
              overwrite = TRUE)
  out_path
}

# ── Cumulative products per district ─────────────────────────────────────────

build_cumulative <- function(date_tifs, dist_name, out_dir, run_tag,
                              dnbr_burn_min) {
  if (length(date_tifs) == 0) return(invisible(NULL))
  safe <- gsub("[^A-Za-z0-9_]", "_", toupper(dist_name))

  # Initialise cumulative rasters from first available tile
  ref        <- rast(date_tifs[1])[[1]]
  first_doy  <- rast(ref); values(first_doy) <- NA_real_
  cum_count  <- rast(ref); values(cum_count)  <- 0L
  max_dnbr   <- rast(ref); values(max_dnbr)   <- NA_real_

  for (tif in date_tifs) {
    date_str  <- regmatches(basename(tif), regexpr("[0-9]{8}(?=_dnbr)", basename(tif), perl=TRUE))
    doy       <- yday(as.Date(date_str, "%Y%m%d"))
    dnbr      <- rast(tif)[[1]]
    if (!compareGeom(dnbr, ref, stopOnError=FALSE)) dnbr <- resample(dnbr, ref, method="bilinear")

    is_burned  <- (!is.na(dnbr)) & (dnbr >= dnbr_burn_min)
    first_doy  <- ifel(is_burned & is.na(first_doy), doy, first_doy)
    cum_count  <- ifel(is_burned, cum_count + 1L, cum_count)
    max_dnbr   <- ifel(!is.na(dnbr) & (is.na(max_dnbr) | dnbr > max_dnbr), dnbr, max_dnbr)
  }
  cum_count[cum_count == 0L] <- NA

  write_r <- function(r, label) {
    p <- file.path(out_dir, glue("{run_tag}_{safe}_{label}.tif"))
    writeRaster(r, p, datatype = "FLT4S",
                gdal = c("COMPRESS=DEFLATE", "TILED=YES",
                         "BLOCKXSIZE=256", "BLOCKYSIZE=256"),
                overwrite = TRUE)
    gcs_push(p, cfg = CFG, subdir = "geotiff")
    p
  }
  write_r(first_doy, "first_burn_doy")
  write_r(cum_count, "cumulative_count")
  write_r(max_dnbr,  "max_dnbr")
  invisible(TRUE)
}

# ── District worker (parallel via furrr) ─────────────────────────────────────

process_district <- function(dist_name, dist_sf, mosaic_paths,
                              out_dir, run_tag, target_crs, dnbr_burn_min) {
  source(file.path(Sys.getenv("PIPELINE_ROOT"), "R", "config.R"))
  source(file.path(Sys.getenv("PIPELINE_ROOT"), "R", "00_gcs_utils.R"))
  suppressPackageStartupMessages({
    library(terra); library(sf); library(lubridate)
    library(glue); library(logger); library(fs)
  })
  terraOptions(memmax = CFG$terra_mem_gb,
               tempdir = (function() { d <- file.path(Sys.getenv("PIPELINE_ROOT"), CFG$dir_tmp, paste0("worker_", Sys.getpid())); dir.create(d, showWarnings = FALSE, recursive = TRUE); d })(),
               todisk = CFG$terra_todisk, progress = 0)

  date_tifs <- character(0)
  for (mp in mosaic_paths) {
    out <- tryCatch(
      clip_district(mp, dist_name, dist_sf, out_dir, run_tag, target_crs),
      error = function(e) { log_error("[{dist_name}] clip error: {e$message}"); NULL }
    )
    if (!is.null(out)) date_tifs <- c(date_tifs, out)
  }

  if (length(date_tifs) > 0) {
    date_tifs_sorted <- date_tifs[order(regmatches(basename(date_tifs),
                                  regexpr("[0-9]{8}(?=_dnbr)", basename(date_tifs), perl=TRUE)))]
    build_cumulative(date_tifs_sorted, dist_name, out_dir, run_tag, dnbr_burn_min)
  }
  # Wipe terra scratch for this district before next one starts (prevents disk fill)
  unlink(list.files(terraOptions(print = FALSE)$tempdir, pattern = "^spat_", full.names = TRUE), force = TRUE)
  date_tifs
}

# ── Entrypoint ────────────────────────────────────────────────────────────────

run_mosaic_and_clip <- function(root_dir, cfg = NULL) {
  source(file.path(root_dir, "R", "config.R"))
  source(file.path(root_dir, "R", "00_gcs_utils.R"))
  terraOptions(memmax = CFG$terra_mem_gb,
               tempdir = file.path(root_dir, CFG$dir_tmp),
               todisk  = CFG$terra_todisk, progress = 0)

  log_appender(appender_tee(
    file.path(root_dir, CFG$dir_logs, paste0(CFG$run_tag, "_03_mosaic.log"))
  ))
  log_threshold(INFO)
  log_info("=== Step 03: Mosaic & clip to districts (parallel) | {CFG$run_id} ===")

  dnbr_dir <- file.path(root_dir, CFG$dir_dnbr)
  out_dir  <- file.path(root_dir, CFG$dir_out_tif)
  dir_create(out_dir)

  # ── Load districts ─────────────────────────────────────────────────────────
  # FIX: layer name from CFG, st_set_crs removed, trust GPKG's stored CRS.
  shp_path <- file.path(root_dir, CFG$shapefile_path, CFG$gpkg_file)
  dists_sf <- st_read(shp_path, layer = CFG$gpkg_lyr_dist, quiet = TRUE) |>
    st_make_valid() |>
    st_transform(4326)

  dist_col <- CFG$dist_col
  if (!dist_col %in% names(dists_sf)) {
    # Fallback to auto-discovery in case the GPKG has a different column name
    dist_col <- intersect(c("DISTRICT","district","District","NAME","name"),
                          names(dists_sf))[1]
    if (is.na(dist_col)) stop("Cannot find district name column. ",
                              "CFG$dist_col=", CFG$dist_col, " not in: ",
                              paste(names(dists_sf), collapse=", "))
    log_warn("CFG$dist_col '{CFG$dist_col}' not found; using fallback '{dist_col}'")
  }
  district_names <- unique(dists_sf[[dist_col]])
  log_info("{length(district_names)} district(s) loaded")

  # ── Mosaic tiles per date (sequential — fast, just merging local files) ────
  dnbr_files <- list.files(dnbr_dir, pattern = "_dnbr\\.tif$")
  date_strs  <- sort(unique(regmatches(dnbr_files,
                              regexpr("[0-9]{8}(?=_[A-Z0-9]{5}_dnbr)", dnbr_files, perl=TRUE))))
  log_info("{length(date_strs)} unique dates to mosaic")

  tmp_mosaic_dir <- file.path(root_dir, CFG$dir_tmp, "mosaics")
  dir_create(tmp_mosaic_dir)

  mosaic_paths <- Filter(Negate(is.null), lapply(date_strs, function(ds) {
    out_mp <- file.path(tmp_mosaic_dir, glue("{CFG$run_tag}_{ds}_mosaic.tif"))
    if (file.exists(out_mp)) return(out_mp)

    m <- mosaic_date_tiles(dnbr_dir, ds, CFG$run_tag)
    if (is.null(m)) return(NULL)
    writeRaster(m, out_mp, datatype = "FLT4S",
                gdal = c("COMPRESS=DEFLATE", "TILED=YES",
                         "BLOCKXSIZE=512", "BLOCKYSIZE=512"),
                overwrite = TRUE)
    log_info("Mosaic {ds} → {basename(out_mp)}")
    out_mp
  }))
  log_info("{length(mosaic_paths)} date mosaics ready")

  # ── Parallel clip per district ─────────────────────────────────────────────
  log_info("Clipping to {length(district_names)} districts (workers: {CFG$n_workers})...")
  options(parallelly.makeNodePSOCK.connectTimeout = 120, parallelly.makeNodePSOCK.timeout = 900)
  plan(multisession, workers = CFG$n_workers)
  on.exit(plan(sequential), add = TRUE)

  dist_results <- future_map(
    district_names,
    function(dn) {
      dist_sf <- dists_sf[dists_sf[[dist_col]] == dn, ]
      process_district(dn, dist_sf, mosaic_paths, out_dir,
                       CFG$run_tag, CFG$target_crs, CFG$dnbr_burn_min)
    },
    .options = furrr_options(seed = TRUE,
      packages = c("terra","sf","lubridate","glue","logger","fs"))
  )

  total_tifs <- sum(vapply(dist_results, length, integer(1)))
  log_info("Step 03 complete. {total_tifs} district GeoTIFFs written.")
  invisible(dist_results)
}

if (!interactive() && identical(commandArgs(TRUE)[1], "--step=03")) {
  root_dir <- normalizePath(Sys.getenv("PIPELINE_ROOT", "."))
  run_mosaic_and_clip(root_dir)
}
