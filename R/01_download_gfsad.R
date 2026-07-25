# =============================================================================
# 01_download_gfsad.R  (v5 — NASA Earthdata Cloud, direct HTTPS)
# =============================================================================
# Downloads GFSAD30SAAFGIRCE cropland-extent tiles from NASA Earthdata Cloud
# and builds a 20 m crop mask for the state AOI.
#
# WHY THIS CHANGED FROM v4:
#   LP DAAC retired Data Pool distribution for MEaSUREs datasets on Dec 3, 2025.
#   The old URL `e4ftl01.cr.usgs.gov/MEASURES/GFSAD30SAAFGIRCE.001/2015.01.01/`
#   no longer exists. Granules are now served from
#   `data.lpdaac.earthdatacloud.nasa.gov` and indexed via CMR.
#
# GFSAD30 IS STATIC (2015 baseline product, never updated). So we:
#   1. Download tiles to disk ONCE — they live forever
#   2. Skip re-download on every pipeline run
#   3. Push to GCS as a permanent asset for re-provisioning
#
# Outputs:
#   data/raw/gfsad/tiles/<tile-files>.tif         persistent — shared across runs
#   data/raw/gfsad/<run_tag>_gfsad30_raw.vrt      transient — per-run merge
#   data/raw/gfsad/<run_tag>_gfsad30_20m.tif      transient — per-run 20m mask
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(httr2)
  library(glue)
  library(fs)
  library(logger)
})

# =============================================================================
# CONSTANTS
# =============================================================================

# All GFSAD30SAAFGIRCE tiles were processed in the same batch in 2017.
# The processing timestamp is shared across every tile.
GFSAD_TIMESTAMP <- "2017286103800"

# Base URL for the migrated Earthdata Cloud hosting (Dec 2025+).
EARTHDATA_CLOUD_BASE <- "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/GFSAD30SAAFGIRCE.001"

# Tiles that exist in the GFSAD30SAAFGIRCE product (South Asia/Afghanistan/Iran).
# Only N20–N30 latitudes by E060–E090 longitudes are relevant for India.
# Listing the ones that are actually published — anything outside this list
# is ocean or outside the product extent.
INDIA_TILES <- c(
  "N10E70", "N10E80",
  "N20E60", "N20E70", "N20E80",
  "N30E60", "N30E70", "N30E80"
)

# =============================================================================
# tile_url()
# Build the direct HTTPS URL for one tile. No listing needed — filenames
# follow a fixed pattern.
# =============================================================================

tile_url <- function(tile_code) {
  granule_id <- sprintf("GFSAD30SAAFGIRCE_2015_%s_001_%s", tile_code, GFSAD_TIMESTAMP)
  paste0(EARTHDATA_CLOUD_BASE, "/", granule_id, "/", granule_id, ".tif")
}

# =============================================================================
# download_tile()
# Stream one tile to disk via Earthdata bearer-token auth.
# Returns the local path on success, NULL on failure (caller can decide to
# tolerate missing tiles).
# =============================================================================

download_tile <- function(url, dest_path, token) {
  if (file.exists(dest_path)) {
    log_info("  Cached: {basename(dest_path)}")
    return(dest_path)
  }

  log_info("  Downloading: {basename(dest_path)} ...")
  tmp <- paste0(dest_path, ".tmp")

  resp <- tryCatch(
    request(url) |>
      req_headers(Authorization = paste("Bearer", token)) |>
      req_timeout(300) |>
      req_retry(max_tries = 3, backoff = ~ 30) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform(path = tmp),
    error = function(e) {
      unlink(tmp)
      log_warn("  Network error for {basename(dest_path)}: {e$message}")
      NULL
    }
  )

  if (is.null(resp)) return(NULL)

  status <- resp_status(resp)
  if (status == 404) {
    unlink(tmp)
    log_info("  Tile not in product (404 — ocean / outside extent)")
    return(NULL)
  }
  if (status != 200) {
    unlink(tmp)
    log_warn("  HTTP {status} for {basename(dest_path)}")
    return(NULL)
  }

  file.rename(tmp, dest_path)
  log_info("  Done: {basename(dest_path)} ({round(file.size(dest_path)/1e6, 1)} MB)")
  dest_path
}

# =============================================================================
# build_crop_mask()
# Merges downloaded tiles into a VRT, then reprojects + resamples to 20 m
# in the target state UTM, clipped to the AOI bounding box.
# GFSAD30 class 2 = irrigated/rainfed cropland in South Asia.
# =============================================================================

build_crop_mask <- function(tile_paths, aoi_sf, out_raw_vrt, out_20m_tif,
                             target_crs, crop_vals = c(2L)) {

  log_info("Building VRT from {length(tile_paths)} tile(s)...")
  v <- terra::vrt(tile_paths, out_raw_vrt, overwrite = TRUE)
  log_info("VRT extent: {paste(as.vector(ext(v)), collapse=', ')}")

  # Crop to AOI bbox in source CRS (native of GFSAD is EPSG:4326)
  aoi_wgs84  <- sf::st_transform(aoi_sf, 4326)
  aoi_bbox   <- as.numeric(sf::st_bbox(aoi_wgs84))
  buf_deg    <- 0.1  # ~10 km buffer to avoid edge artifacts after reprojection
  ext_clip   <- terra::ext(aoi_bbox[1] - buf_deg, aoi_bbox[3] + buf_deg,
                            aoi_bbox[2] - buf_deg, aoi_bbox[4] + buf_deg)
  v_clip <- terra::crop(v, ext_clip)

  # Reproject to target UTM at 20 m
  log_info("Reprojecting GFSAD30 to {target_crs} at 20 m...")
  v_utm <- terra::project(v_clip, target_crs, method = "near",
                           res = 20, threads = TRUE)

  # Binarize: 1 where cropland, 0 elsewhere
  log_info("Binarizing to cropland mask (crop classes: {paste(crop_vals, collapse=',')})...")
  m_bin <- terra::ifel(v_utm %in% crop_vals, 1L, 0L)

  terra::writeRaster(
    m_bin, out_20m_tif,
    datatype  = "INT1U",
    gdal      = c("COMPRESS=DEFLATE", "TILED=YES",
                  "BLOCKXSIZE=512", "BLOCKYSIZE=512"),
    overwrite = TRUE
  )
  log_info("Crop mask written: {out_20m_tif}")
  out_20m_tif
}

# =============================================================================
# run_step_01()  — main entry point called by run_pipeline.R
# =============================================================================

run_step_01 <- function(root_dir, cfg) {

  log_info("=== Step 01: Download GFSAD30 crop mask (Earthdata Cloud) ===")

  out_dir   <- file.path(root_dir, cfg$dir_raw_gfsad)
  tile_dir  <- file.path(out_dir, "tiles")
  fs::dir_create(c(out_dir, tile_dir))

  out_raw_vrt <- file.path(out_dir, glue("{cfg$run_tag}_gfsad30_raw.vrt"))
  out_20m_tif <- file.path(out_dir, glue("{cfg$run_tag}_gfsad30_20m.tif"))

  if (file.exists(out_20m_tif)) {
    log_info("GFSAD30 mask already exists for {cfg$run_tag} — skipping.")
    log_info("  {out_20m_tif}")
    return(out_20m_tif)
  }

  # ── Load AOI ────────────────────────────────────────────────────────────────
  gpkg_path <- file.path(root_dir, cfg$shapefile_path, cfg$gpkg_file)
  aoi_sf <- sf::st_read(gpkg_path, layer = cfg$gpkg_lyr_state, quiet = TRUE)
  aoi_sf <- sf::st_make_valid(aoi_sf)

  # ── Determine which tiles overlap the state AOI ─────────────────────────────
  aoi_bbox <- as.numeric(sf::st_bbox(sf::st_transform(aoi_sf, 4326)))
  lon_min <- floor(aoi_bbox[1] / 10) * 10
  lon_max <- floor(aoi_bbox[3] / 10) * 10
  lat_min <- floor(aoi_bbox[2] / 10) * 10
  lat_max <- floor(aoi_bbox[4] / 10) * 10

  needed_tiles <- character(0)
  for (lat in seq(lat_min, lat_max, by = 10)) {
    for (lon in seq(lon_min, lon_max, by = 10)) {
      needed_tiles <- c(needed_tiles, sprintf("N%02dE%d", lat, lon))
    }
  }
  needed_tiles <- intersect(needed_tiles, INDIA_TILES)
  log_info("AOI tiles needed: {paste(needed_tiles, collapse=', ')}")

  if (length(needed_tiles) == 0)
    stop("No GFSAD30 tiles intersect the AOI. Check the state boundary CRS.")

  # ── Get Earthdata token ─────────────────────────────────────────────────────
  earthdata_token <- Sys.getenv("EARTHDATA_TOKEN", "")
  if (nchar(earthdata_token) == 0)
    stop("EARTHDATA_TOKEN not set in ~/.Renviron")

  # ── Download missing tiles (cached across runs) ─────────────────────────────
  log_info("Downloading from {EARTHDATA_CLOUD_BASE}...")
  tile_paths <- character(0)

  for (tile in needed_tiles) {
    url  <- tile_url(tile)
    dest <- file.path(tile_dir, basename(url))
    path <- tryCatch(
      download_tile(url, dest, earthdata_token),
      error = function(e) { log_warn("Skipping {tile}: {e$message}"); NULL }
    )
    if (!is.null(path)) tile_paths <- c(tile_paths, path)
  }

  if (length(tile_paths) == 0)
    stop("All tile downloads failed. Check EARTHDATA_TOKEN and LP DAAC Data Pool app authorization.")

  log_info("{length(tile_paths)} tile(s) ready locally.")

  # ── Build the 20 m crop mask ────────────────────────────────────────────────
  build_crop_mask(
    tile_paths  = tile_paths,
    aoi_sf      = aoi_sf,
    out_raw_vrt = out_raw_vrt,
    out_20m_tif = out_20m_tif,
    target_crs  = cfg$target_crs,
    crop_vals   = cfg$gfsad_crop_vals
  )

  # ── Push to GCS (optional — for cross-run persistence) ──────────────────────
  if (nchar(cfg$gcs_bucket) > 0 && exists("gcs_push", mode = "function")) {
    tryCatch({
      gcs_push(out_20m_tif, cfg = cfg, subdir = "gfsad")
    }, error = function(e) {
      log_warn("GCS push failed (non-fatal): {e$message}")
    })
  }

  log_info("Step 01 complete.")
  invisible(out_20m_tif)
}

# Allow standalone invocation: Rscript R/01_download_gfsad.R --step=01
if (!interactive() && identical(commandArgs(TRUE)[1], "--step=01")) {
  root_dir <- normalizePath(Sys.getenv("PIPELINE_ROOT", "."))
  source(file.path(root_dir, "R", "config.R"))
  run_step_01(root_dir, CFG)
}
