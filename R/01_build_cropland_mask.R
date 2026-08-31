# =============================================================================
# 01_build_cropland_mask.R  (v6 — ESA WorldCover v200)
# =============================================================================
# Builds the 20 m cropland mask applied to dNBR in Step 02.
#
# WHY THIS REPLACED GFSAD30 (01_download_gfsad.R, removed):
#   GFSAD30SAAFGIRCE is a 2015 baseline product that is never updated, so
#   every season from 2024 onward was being masked against ten-year-old
#   cropland. It also required an EARTHDATA_TOKEN and LP DAAC app
#   authorisation, and LP DAAC retired Data Pool distribution in Dec 2025,
#   forcing a migration to Earthdata Cloud that added another failure mode.
#
#   More importantly, the analysis layer masks the same rasters a second
#   time with WorldCover. Two stacked cropland definitions mean the reported
#   cropland is GFSAD ∩ WorldCover ∩ valid-observation, and no alternative
#   mask can ever add cropland that GFSAD excluded. One definition, applied
#   once, here.
#
# ESA WorldCover v200:
#   - 2021 epoch, 3-degree tiles, EPSG:4326, no authentication
#   - class 40 = Cropland
#   - Grassland (30) is deliberately excluded: in the IGP it picks up fallow
#     and field margins that inflate the cropland base.
#
# CACHING:
#   Tiles are static and shared across every run and every state — they are
#   downloaded once into tiles/ and reused. Only the reprojected per-run
#   mask is rebuilt.
#
# Outputs:
#   data/raw/cropland_mask/tiles/ESA_WorldCover_*.tif   persistent, shared
#   data/raw/cropland_mask/<run_tag>_worldcover.vrt     transient, per-run
#   data/raw/cropland_mask/<run_tag>_cropland_20m.tif   transient, per-run
#
# CONTRACT WITH STEP 02:
#   Step 02 applies the mask as `dnbr[crop_mask == 0L] <- NA`, so an NA in
#   the mask leaves the dNBR pixel UNMASKED. The output of this step must
#   therefore be strictly 0/1 with no NA anywhere in the AOI. Enforced below.
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
# worldcover_tile_name()
# 3-degree tile filename. lat0/lon0 are the SW corner, multiples of 3.
# =============================================================================

worldcover_tile_name <- function(lon0, lat0, year, version) {
  ns <- if (lat0 >= 0) sprintf("N%02d", lat0) else sprintf("S%02d", abs(lat0))
  ew <- if (lon0 >= 0) sprintf("E%03d", lon0) else sprintf("W%03d", abs(lon0))
  sprintf("ESA_WorldCover_10m_%d_%s_%s%s_Map.tif", year, version, ns, ew)
}

# =============================================================================
# required_tiles()
# Tiles whose 3-degree footprint intersects the AOI POLYGON (not its bbox).
# Intersecting the polygon saves 1-2 downloads on diagonal states and avoids
# fetching tiles that contribute nothing.
# =============================================================================

required_tiles <- function(aoi_sf, cfg) {
  d  <- cfg$wc_tile_deg
  bb <- as.numeric(sf::st_bbox(sf::st_transform(aoi_sf, 4326)))

  lon0 <- seq(floor(bb[1] / d) * d, floor(bb[3] / d) * d, by = d)
  lat0 <- seq(floor(bb[2] / d) * d, floor(bb[4] / d) * d, by = d)
  grid <- expand.grid(lon0 = lon0, lat0 = lat0)

  polys <- lapply(seq_len(nrow(grid)), function(i) {
    x0 <- grid$lon0[i]; y0 <- grid$lat0[i]
    sf::st_polygon(list(cbind(c(x0, x0 + d, x0 + d, x0,     x0),
                              c(y0, y0,     y0 + d, y0 + d, y0))))
  })

  tiles <- sf::st_sf(grid, geometry = sf::st_sfc(polys, crs = 4326))
  aoi   <- sf::st_transform(sf::st_union(aoi_sf), 4326)
  tiles <- tiles[lengths(sf::st_intersects(tiles, aoi)) > 0, ]

  if (nrow(tiles) == 0)
    stop("No WorldCover tiles intersect the AOI. Check the state boundary CRS.")

  tiles$fname <- mapply(worldcover_tile_name, tiles$lon0, tiles$lat0,
                        MoreArgs = list(year = cfg$wc_year,
                                        version = cfg$wc_version))
  tiles
}

# =============================================================================
# download_tile()
# No authentication — the WorldCover bucket is public. A missing tile is
# fatal, not tolerated: a silently absent tile is a hole in the cropland
# mask, which becomes a hole in every downstream burnt-area figure.
# =============================================================================

download_tile <- function(url, dest_path) {
  if (file.exists(dest_path) && file.size(dest_path) > 0) {
    log_info("  Cached: {basename(dest_path)}")
    return(dest_path)
  }

  log_info("  Downloading: {basename(dest_path)} ...")
  tmp <- paste0(dest_path, ".tmp")

  ok <- tryCatch({
    request(url) |>
      req_timeout(3600) |>
      req_retry(max_tries = 3, backoff = ~ 30) |>
      req_perform(path = tmp)
    TRUE
  }, error = function(e) {
    unlink(tmp)
    log_error("  Failed: {basename(dest_path)} — {e$message}")
    FALSE
  })

  if (!ok || !file.exists(tmp) || file.size(tmp) == 0) {
    unlink(tmp)
    stop("WorldCover tile could not be retrieved: ", basename(dest_path),
         "\nURL: ", url,
         "\nIf this host is unreachable, place the tile manually in ",
         dirname(dest_path), " and re-run.")
  }

  file.rename(tmp, dest_path)
  log_info("  Done: {basename(dest_path)} ({round(file.size(dest_path)/1e6, 1)} MB)")
  dest_path
}

# =============================================================================
# build_crop_mask()
# Mosaic tiles (all EPSG:4326, so a VRT is valid), clip to the AOI bbox,
# reproject to the state UTM at 20 m, binarise to strict 0/1.
#
# cropland_resample:
#   "near"     one 10 m subpixel per 20 m cell. Fast, matches v6 behaviour.
#   "fraction" area-weighted cropland fraction, kept at >= wc_min_fraction.
#              Stricter; drops mixed field-edge pixels. Slower.
# =============================================================================

build_crop_mask <- function(tile_paths, aoi_sf, out_vrt, out_tif, cfg) {

  log_info("Building VRT from {length(tile_paths)} tile(s)...")
  v <- terra::vrt(tile_paths, out_vrt, overwrite = TRUE)

  aoi_bbox <- as.numeric(sf::st_bbox(sf::st_transform(aoi_sf, 4326)))
  buf      <- 0.1  # ~10 km, avoids edge artefacts after reprojection
  v_clip   <- terra::crop(v, terra::ext(aoi_bbox[1] - buf, aoi_bbox[3] + buf,
                                        aoi_bbox[2] - buf, aoi_bbox[4] + buf))

  log_info("Extracting class {cfg$wc_crop_class} (Cropland)...")
  is_crop <- terra::ifel(v_clip == cfg$wc_crop_class, 1L, 0L)

  log_info("Reprojecting to {cfg$target_crs} at {cfg$native_res_m} m ({cfg$cropland_resample})...")
  if (identical(cfg$cropland_resample, "fraction")) {
    frac  <- terra::project(is_crop, cfg$target_crs, method = "average",
                            res = cfg$native_res_m, threads = TRUE)
    m_bin <- terra::ifel(frac >= cfg$wc_min_fraction, 1L, 0L)
  } else {
    m_bin <- terra::project(is_crop, cfg$target_crs, method = "near",
                            res = cfg$native_res_m, threads = TRUE)
  }

  # Step 02 does `dnbr[crop_mask == 0L] <- NA`, so NA in the mask means the
  # pixel is NOT masked. Force strict 0/1 — reprojection can leave NA at the
  # edges of the transformed footprint.
  m_bin[is.na(m_bin)] <- 0L

  n_crop <- terra::global(m_bin, "sum", na.rm = TRUE)[1, 1]
  if (!is.finite(n_crop) || n_crop == 0)
    stop("Cropland mask contains no cropland pixels. ",
         "Check wc_crop_class (", cfg$wc_crop_class, ") and the tile set.")

  pct <- 100 * n_crop / terra::ncell(m_bin)
  log_info("Cropland pixels: {format(n_crop, big.mark=',')} ({round(pct,1)}% of mask extent)")
  if (pct < 5)
    log_warn("Cropland is under 5% of the mask extent — verify the class value.")

  terra::writeRaster(
    m_bin, out_tif,
    datatype  = "INT1U",
    gdal      = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES",
                  "BLOCKXSIZE=512", "BLOCKYSIZE=512", "BIGTIFF=YES"),
    overwrite = TRUE
  )
  log_info("Cropland mask written: {out_tif}")
  out_tif
}

# =============================================================================
# run_build_cropland_mask()  — entry point called by run_pipeline.R
# =============================================================================

run_build_cropland_mask <- function(root_dir, cfg) {

  log_info("=== Step 01: Build ESA WorldCover {cfg$wc_version} cropland mask ===")

  out_dir  <- file.path(root_dir, cfg$dir_cropland_mask)
  tile_dir <- file.path(out_dir, "tiles")
  fs::dir_create(c(out_dir, tile_dir))

  out_vrt <- file.path(out_dir, glue("{cfg$run_tag}_worldcover.vrt"))
  out_tif <- file.path(out_dir, glue("{cfg$run_tag}_cropland_20m.tif"))

  if (file.exists(out_tif)) {
    log_info("Cropland mask already exists for {cfg$run_tag} — skipping.")
    log_info("  {out_tif}")
    return(invisible(out_tif))
  }

  gpkg_path <- file.path(root_dir, cfg$shapefile_path, cfg$gpkg_file)
  aoi_sf <- sf::st_read(gpkg_path, layer = cfg$gpkg_lyr_state, quiet = TRUE) |>
            sf::st_make_valid()

  tiles <- required_tiles(aoi_sf, cfg)
  log_info("AOI tiles needed ({nrow(tiles)}): {paste(tiles$fname, collapse=', ')}")

  tile_paths <- vapply(tiles$fname, function(fn) {
    download_tile(file.path(cfg$wc_base_url, fn), file.path(tile_dir, fn))
  }, character(1), USE.NAMES = FALSE)

  log_info("{length(tile_paths)} tile(s) ready locally.")

  build_crop_mask(tile_paths, aoi_sf, out_vrt, out_tif, cfg)

  if (nchar(cfg$gcs_bucket) > 0 && exists("gcs_push", mode = "function")) {
    tryCatch(gcs_push(out_tif, cfg = cfg, subdir = "cropland_mask"),
             error = function(e) log_warn("GCS push failed (non-fatal): {e$message}"))
  }

  log_info("Step 01 complete.")
  invisible(out_tif)
}

# Allow standalone invocation: Rscript R/01_build_cropland_mask.R --step=01
if (!interactive() && identical(commandArgs(TRUE)[1], "--step=01")) {
  root_dir <- normalizePath(Sys.getenv("PIPELINE_ROOT", "."))
  source(file.path(root_dir, "R", "config.R"))
  run_build_cropland_mask(root_dir, CFG)
}
