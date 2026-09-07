# =============================================================================
# 02_compute_dnbr.R
# Stream Sentinel-2 COGs directly from STAC via /vsicurl/ HTTP range requests.
# No full-tile downloads. Clips to AOI bbox before loading pixels.
# Computes dNBR AND applies the WorldCover crop mask in a single raster pass
# per tile/date.
# Runs in parallel across MGRS tiles using furrr.
# =============================================================================
# v5 changes vs v4:
#   - gcs_push() calls now pass cfg = CFG (FIX B1)
#   - CDSE token is refreshed inside each worker via get_cdse_token()
#     instead of receiving a stale master-process token (FIX B3)
#   - stac_search_window stops() after all retries fail instead of
#     returning NULL silently (FIX A6)
#   - Dead `aoi_sf <- NULL` conditional removed (FIX A5)
#   - Hardcoded "district_boundary" replaced by CFG$gpkg_lyr_dist (FIX B2 cont.)
# =============================================================================
# Outputs per tile per post-fire date:
#   data/interim/dnbr/<run_tag>_<YYYYMMDD>_<tile>_dnbr.tif
#     Band 1: dNBR (float32), crop-masked
#     Band 2: severity class (0–4, uint8)
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

suppressPackageStartupMessages({
  library(rstac)
  library(httr2)
  library(sf)
  library(terra)
  library(dplyr)
  library(lubridate)
  library(glue)
  library(fs)
  library(logger)
  library(future)
  library(furrr)
})

# ── SCL valid-pixel classes ───────────────────────────────────────────────────
# 4=Vegetation 5=Not-vegetated 6=Water 7=Unclassified
# Excluded: 0,1,2,3 (no-data/shadow/dark), 8,9,10,11 (cloud/cirrus/snow)
SCL_VALID <- c(4L, 5L, 6L, 7L)

# ── STAC search helpers ───────────────────────────────────────────────────────

stac_search_window <- function(bbox, date_start, date_end, source, token = NULL, retries = 3) {
  last_err <- NULL
  for (attempt in seq_len(retries)) {
    result <- tryCatch(
      stac_search_window_inner(bbox, date_start, date_end, source, token),
      error = function(e) {
        last_err <<- e
        if (grepl("429", paste(e$message, collapse=" ")) && attempt < retries) {
          wait <- 30 * attempt
          message("  [STAC 429] Rate limited. Waiting ", wait, "s before retry ", attempt+1, "/", retries)
          Sys.sleep(wait)
          NULL
        } else stop(e)
      }
    )
    if (!is.null(result)) return(result)
  }
  # FIX A6: fail loudly instead of returning NULL on retry exhaustion
  stop("STAC search exhausted ", retries, " retries. ",
       "Last error: ", if (!is.null(last_err)) last_err$message else "unknown")
}

stac_search_window_inner <- function(bbox, date_start, date_end, source, token = NULL) {
  dt_range <- paste0(date_start, "T00:00:00Z/", date_end, "T23:59:59Z")
  max_cloud <- CFG$max_cloud_pct

  if (source == "MPC") {
    all_items <- stac(CFG$mpc_stac_url) |>
      stac_search(collections = CFG$mpc_collection, bbox = bbox,
                  datetime = dt_range, limit = 500) |>
      post_request() |>
      items_fetch(progress = FALSE)
    items <- all_items
    items$features <- Filter(function(f) {
      cc <- f$properties[["eo:cloud_cover"]]
      is.null(cc) || cc <= max_cloud
    }, all_items$features)
  } else {
    items <- stac(CFG$cdse_stac_url) |>
      # CDSE caps sentinel-2-l2a at 200 per page unless the `fields`
      # extension is used; 500 returns HTTP 400 LimitValidationError.
      # items_fetch() paginates, so this only sets page size.
      stac_search(collections = CFG$cdse_collection, bbox = bbox,
                  datetime = dt_range, limit = 100) |>
      ext_filter(`eo:cloud_cover` <= max_cloud &
                 `s2:processing_baseline` >= "05.00") |>
      get_request() |>
      items_fetch(progress = FALSE)
  }
  items$features
}

parse_item_meta <- function(item, source) {
  dt  <- item$properties$datetime
  tile_id <- item$properties[["s2:mgrs_tile"]] %||%
             sub(".*_T([0-9A-Z]{5})_.*", "\\1", item$id)
  list(id = item$id, date = as.Date(substr(dt, 1, 10)),
       tile_id = tile_id,
       cloud   = item$properties[["eo:cloud_cover"]] %||% NA_real_)
}

get_asset_url <- function(item, band, source) {
  keys <- c(band, tolower(band))
  for (k in keys) if (k %in% names(item$assets)) return(item$assets[[k]]$href)
  NULL
}

sign_item <- function(item, source) {
  if (source == "MPC")
    rstac::items_sign(item, sign_fn = rstac::sign_planetary_computer())
  else
    item
}

# ── Streaming raster loader ───────────────────────────────────────────────────

#' Load a single band from a COG URL, clipped to the AOI.
#' Uses /vsicurl/ — GDAL only fetches the required HTTP byte ranges.
#'
#' FIX 2026-09: the clip window is now derived from the AOI polygon in
#' EPSG:4326 and projected into THIS raster's native CRS. Previously a single
#' bounding box was computed once in CFG$target_crs (UTM 44N for UP) and
#' reused for every tile. A rectangle in 44N is not a rectangle in 43N: 4
#' degrees west of the 44N central meridian the edges bow inward by
#' kilometres, so western tiles were cropped to a sliver. 43RFP came out
#' 779 columns wide instead of 5490 — a 15.6 km strip of a 110 km tile.
#' Projecting the real polygon (many vertices) instead of 4 bbox corners
#' also removes the corner-only densification error.
stream_band_clipped <- function(url, clip_aoi_wkt, target_crs = NULL,
                                 scale = 1e-4, bearer_token = NULL,
                                 tile_id = NA_character_) {
  if (!is.null(bearer_token))
    Sys.setenv(GDAL_HTTP_HEADERS = paste0("Authorization: Bearer ", bearer_token))

  r <- tryCatch(
    rast(paste0("/vsicurl/", url)),
    error = function(e) {
      log_warn("  [stream fail] tile {tile_id}: {basename(url)}: {e$message}")
      NULL
    }
  )
  if (is.null(r)) return(NULL)

  clip_native <- tryCatch(
    project(vect(clip_aoi_wkt, crs = "EPSG:4326"), crs(r)),
    error = function(e) {
      log_warn("  [clip project fail] tile {tile_id}: {e$message}")
      NULL
    }
  )
  if (is.null(clip_native)) return(NULL)

  # A tile that does not actually intersect the AOI is a caller error, not a
  # data problem — surface it rather than returning an empty raster.
  if (is.null(intersect(ext(r), ext(clip_native)))) {
    log_warn("  [no AOI overlap] tile {tile_id}: {basename(url)}")
    return(NULL)
  }

  r_clip <- tryCatch(
    crop(r, clip_native),
    error = function(e) {
      log_warn("  [crop fail] tile {tile_id}: {e$message}")
      NULL
    }
  )
  if (is.null(r_clip)) return(NULL)
  if (ncell(r_clip) == 0) {
    log_warn("  [empty crop] tile {tile_id}: {basename(url)}")
    return(NULL)
  }

  if (scale != 1) r_clip <- r_clip * scale
  r_clip
}

# ── NBR & dNBR computation ────────────────────────────────────────────────────

compute_nbr <- function(b8a, b12) (b8a - b12) / (b8a + b12)

classify_severity <- function(dnbr) {
  classify(dnbr,
    rcl = cbind(
      from  = CFG$severity_breaks[-length(CFG$severity_breaks)],
      to    = CFG$severity_breaks[-1],
      value = seq_along(CFG$severity_labels) - 1L
    ),
    include.lowest = TRUE
  )
}

valid_frac_from_scl <- function(scl_r) {
  v <- values(scl_r, na.rm = FALSE)
  sum(v %in% SCL_VALID, na.rm = TRUE) / length(v[!is.na(v)])
}

# ── Per-tile baseline builder ─────────────────────────────────────────────────

#' Build median-composite baseline NBR from the N best (least-cloudy) baseline
#' acquisitions for a given MGRS tile, streaming directly from STAC.
build_tile_baseline <- function(tile_items, n_img, clip_aoi_wkt, target_crs,
                                 token, baseline_dir, run_tag, tile_id) {
  out_path <- file.path(baseline_dir, glue("{run_tag}_{tile_id}_baseline.tif"))
  if (file.exists(out_path)) {
    log_debug("  [skip] baseline for tile {tile_id}")
    return(out_path)
  }

  # Sort by cloud cover ascending, pick best n_img
  clouds <- vapply(tile_items, function(it) it$cloud, numeric(1))
  best   <- tile_items[order(clouds)[seq_len(min(n_img, length(tile_items)))]]
  dates  <- vapply(best, function(it) as.character(it$date), character(1))
  log_info("  Tile {tile_id} baseline: {length(best)} image(s): {paste(dates,collapse=', ')}")

  nbr_layers <- lapply(best, function(it) {
    raw_it <- sign_item(it$item, CFG$stac_source)
    b8a <- stream_band_clipped(get_asset_url(raw_it, "B8A", CFG$stac_source),
                                clip_aoi_wkt, bearer_token = token, tile_id = tile_id)
    b12 <- stream_band_clipped(get_asset_url(raw_it, "B12", CFG$stac_source),
                                clip_aoi_wkt, bearer_token = token, tile_id = tile_id)
    scl <- stream_band_clipped(get_asset_url(raw_it, "SCL", CFG$stac_source),
                                clip_aoi_wkt, scale = 1,
                                bearer_token = token, tile_id = tile_id)
    if (is.null(b8a) || is.null(b12) || is.null(scl)) return(NULL)

    # Align to same extent/res before combining
    b12 <- resample(b12, b8a, method = "bilinear")
    scl <- resample(scl, b8a, method = "near")

    nbr <- compute_nbr(b8a, b12)
    nbr[!(scl %in% SCL_VALID)] <- NA
    nbr
  })
  nbr_layers <- Filter(Negate(is.null), nbr_layers)
  if (length(nbr_layers) == 0) stop("No valid baseline images for tile ", tile_id)

  baseline <- if (length(nbr_layers) == 1) {
    nbr_layers[[1]]
  } else {
    app(rast(nbr_layers), median, na.rm = TRUE)
  }

  writeRaster(baseline, out_path,
              datatype = "FLT4S",
              gdal = c("COMPRESS=DEFLATE", "TILED=YES",
                       "BLOCKXSIZE=512", "BLOCKYSIZE=512"),
              overwrite = TRUE)
  log_info("  Baseline written → {basename(out_path)}")
  gcs_push(out_path, cfg = CFG, subdir = "baselines")  # FIX B1: pass cfg
  out_path
}

# ── Cropland mask alignment ──────────────────────────────────────────────────

#' Align the cropland mask onto a dNBR tile's grid.
#'
#' FIX 2026-09: the mask is built once in CFG$target_crs (32644 for UP), but
#' dNBR tiles keep their NATIVE MGRS zone — 43* is 32643, 45* is 32645. The
#' old code called resample() alone, which matches grids but does NOT
#' reproject, so:
#'   zone 43 — numeric eastings overlap, so the mask was sampled ~380 km from
#'             the true location. Mostly 0, so `dnbr[crop_mask == 0L] <- NA`
#'             wiped the tile. 43RGP came out with 10 valid pixels.
#'   zone 45 — no numeric overlap, so the mask returned all NA. `NA == 0L` is
#'             NA, not TRUE, so NOTHING was masked and those tiles were never
#'             cropland-filtered at all — a silent failure that looks like
#'             success.
#'   zone 44 — correct, which is why only the middle of the state worked.
#'
#' project() when the CRS differs, resample() only when it matches. The mask
#' is cropped to the target footprint in its own CRS first so a tile-sized
#' window is reprojected rather than the whole state.
align_crop_mask <- function(crop_mask, dnbr, tile_id = NA_character_,
                            date_str = NA_character_) {
  if (!same.crs(crop_mask, dnbr)) {
    tgt <- project(as.polygons(ext(dnbr), crs = crs(dnbr)), crs(crop_mask))

    # terra 1.9.34: intersect() on two SpatExtents returns an object crop()
    # rejects with "cannot get a SpatExtent from y". Compute the overlap
    # explicitly and rebuild the extent from four numbers.
    e <- ext(crop_mask); t <- ext(tgt)
    xmin <- max(e$xmin, t$xmin); xmax <- min(e$xmax, t$xmax)
    ymin <- max(e$ymin, t$ymin); ymax <- min(e$ymax, t$ymax)
    if (xmax <= xmin || ymax <= ymin) {
      log_warn("  {date_str} tile {tile_id}: cropland mask does not cover this tile")
      return(NULL)
    }

    crop_mask <- project(crop(crop_mask, ext(xmin, xmax, ymin, ymax)),
                         dnbr, method = "near")
  } else if (!compareGeom(crop_mask, dnbr, stopOnError = FALSE)) {
    crop_mask <- resample(crop_mask, dnbr, method = "near")
  }

  # Outside the mask footprint is outside the AOI, i.e. not cropland. Leaving
  # these as NA is what let zone 45 through unmasked.
  crop_mask[is.na(crop_mask)] <- 0L
  crop_mask
}

# ── Per-date dNBR + mask (single pass) ───────────────────────────────────────

#' Stream one post-fire S2 acquisition, compute dNBR, apply cropland mask,
#' classify severity — all in a single raster pass before writing to disk.
#' Returns a list(status=, path=, date=, observed=) rather than a bare path.
#'
#' FIX 2026-09 (a): the gap check now runs AFTER the scene has been shown to
#' be usable, not before. Previously a scene could be rejected on gap grounds
#' without ever being examined, and the caller then had no way to know whether
#' it was observable.
#'
#' FIX 2026-09 (b): `observed` tells the caller whether this acquisition was a
#' usable observation of the ground, independently of whether a dNBR was
#' written. The caller re-anchors its gap clock on any observed date. Without
#' that, one silent failure froze the clock and every later date failed the
#' gap test against a stale anchor, terminating the series permanently — which
#' is why western tiles stopped in mid-March with 24 of 28 scenes unused.
process_postfire_date <- function(item, baseline_path, mask_path,
                                   clip_aoi_wkt, target_crs, out_dir,
                                   run_tag, tile_id, prev_date = NULL,
                                   token = NULL) {
  acq_date <- item$date
  date_str <- format(acq_date, "%Y%m%d")
  out_path <- file.path(out_dir,
    glue("{run_tag}_{date_str}_{tile_id}_dnbr.tif"))

  res <- function(status, path = NULL, observed = FALSE)
    list(status = status, path = path, date = acq_date, observed = observed)

  if (file.exists(out_path)) {
    log_debug("  [cached] {basename(out_path)}")
    return(res("cached", out_path, observed = TRUE))
  }

  raw_item <- sign_item(item$item, CFG$stac_source)

  scl <- stream_band_clipped(get_asset_url(raw_item, "SCL", CFG$stac_source),
                              clip_aoi_wkt, scale = 1,
                              bearer_token = token, tile_id = tile_id)
  if (is.null(scl)) {
    log_warn("  {date_str} tile {tile_id}: SCL unavailable — not observed")
    return(res("stream_fail"))
  }

  vfrac <- valid_frac_from_scl(scl)
  if (!is.finite(vfrac)) {
    log_warn("  {date_str} tile {tile_id}: valid_frac not computable — not observed")
    return(res("stream_fail"))
  }
  if (vfrac < CFG$min_valid_frac) {
    log_warn("  {date_str} tile {tile_id}: valid_frac={round(vfrac,2)} — too cloudy")
    return(res("too_cloudy"))
  }

  # Usable observation. Re-anchor the gap clock even if no dNBR is written.
  if (!is.null(prev_date)) {
    gap <- as.integer(acq_date - prev_date)
    if (gap > CFG$max_gap_days) {
      log_warn("  {date_str} tile {tile_id}: gap {gap}d > {CFG$max_gap_days}d — no dNBR, re-anchoring")
      return(res("skipped_gap", observed = TRUE))
    }
  }

  b8a <- stream_band_clipped(get_asset_url(raw_item, "B8A", CFG$stac_source),
                              clip_aoi_wkt, bearer_token = token, tile_id = tile_id)
  b12 <- stream_band_clipped(get_asset_url(raw_item, "B12", CFG$stac_source),
                              clip_aoi_wkt, bearer_token = token, tile_id = tile_id)
  if (is.null(b8a) || is.null(b12)) {
    log_warn("  {date_str} tile {tile_id}: B8A/B12 unavailable after valid SCL")
    return(res("stream_fail", observed = TRUE))
  }

  b12 <- resample(b12, b8a, method = "bilinear")
  scl <- resample(scl, b8a, method = "near")

  baseline <- rast(baseline_path)
  if (!compareGeom(baseline, b8a, stopOnError = FALSE))
    baseline <- resample(baseline, b8a, method = "bilinear")

  nbr_post <- compute_nbr(b8a, b12)
  nbr_post[!(scl %in% SCL_VALID)] <- NA

  dnbr     <- baseline - nbr_post
  severity <- classify_severity(dnbr)

  # ── Apply cropland mask in same pass ───────────────────────────────────────
  crop_mask <- align_crop_mask(rast(mask_path), dnbr, tile_id, date_str)
  if (is.null(crop_mask)) return(res("mask_miss", observed = TRUE))

  n_crop <- global(crop_mask, "sum", na.rm = TRUE)[1, 1]
  if (!is.finite(n_crop) || n_crop == 0) {
    log_warn("  {date_str} tile {tile_id}: no cropland pixels after mask alignment")
    return(res("mask_empty", observed = TRUE))
  }

  dnbr[crop_mask == 0L]     <- NA
  severity[crop_mask == 0L] <- NA

  out_stack <- c(dnbr, severity)
  names(out_stack) <- c("dNBR", "severity")

  writeRaster(out_stack, out_path,
              datatype = "FLT4S",
              gdal = c("COMPRESS=DEFLATE", "TILED=YES",
                       "BLOCKXSIZE=512", "BLOCKYSIZE=512"),
              overwrite = TRUE)
  log_info(paste0("  → ", basename(out_path), " | valid:", round(vfrac*100), "% | burned:", sum(values(dnbr) >= CFG$dnbr_burn_min, na.rm=TRUE), " px"))
  gcs_push(out_path, cfg = CFG, subdir = "dnbr")  # FIX B1: pass cfg
  res("written", out_path, observed = TRUE)
}

# ── Tile-level worker (runs inside furrr future) ──────────────────────────────

process_tile <- function(tile_id, baseline_items, postfire_items,
                          clip_aoi_wkt, target_crs, mask_path,
                          dnbr_dir, baseline_dir, run_tag, cfg,
                          tile_log_dir = NULL) {

  # Re-source config and credentials inside worker (futures have clean envs)
  CFG <<- cfg
  source(file.path(Sys.getenv("PIPELINE_ROOT"), "R", "config.R"))
  source(file.path(Sys.getenv("PIPELINE_ROOT"), "R", "00_gcs_utils.R"))
  source(file.path(Sys.getenv("PIPELINE_ROOT"), "credentials", "CDSE_api.R"))

  suppressPackageStartupMessages({
    library(terra); library(rstac); library(sf)
    library(lubridate); library(glue); library(logger); library(fs)
  })
  .wtmp <- file.path(Sys.getenv("PIPELINE_ROOT"), CFG$dir_tmp,
                     paste0("worker_", Sys.getpid()))
  dir.create(.wtmp, showWarnings = FALSE, recursive = TRUE)

  # FIX 2026-09: give every tile its own log file. Warnings raised inside a
  # furrr worker are not reliably relayed to the master's appender, which is
  # how 24 of 28 scenes on 43RFP were dropped with nothing in the run log.
  if (!is.null(tile_log_dir)) {
    dir.create(tile_log_dir, showWarnings = FALSE, recursive = TRUE)
    log_appender(appender_file(file.path(tile_log_dir, paste0(tile_id, ".log"))))
    log_threshold(INFO)
  }
  terraOptions(memmax = CFG$terra_mem_gb, tempdir = .wtmp,
    todisk = CFG$terra_todisk, progress = 0)
  on.exit(try(terra::tmpFiles(current = TRUE, remove = TRUE), silent = TRUE),
          add = TRUE)

  # FIX B3: each worker fetches its own CDSE token from the cached helper.
  # Caches with 60s safety margin; refreshes itself when near expiry.
  # Token cache is per-R-process so workers refresh independently of master.
  refresh_token <- function() {
    if (CFG$stac_source == "CDSE") {
      tok <- tryCatch(get_cdse_token(),
                      error = function(e) { log_warn(e$message); NULL })
      set_gdal_streaming_env(tok)
      tok
    } else NULL
  }

  token <- refresh_token()

  b_items <- baseline_items[[tile_id]]
  p_items <- postfire_items[[tile_id]]

  if (is.null(b_items) || length(b_items) == 0) {
    log_warn("[tile {tile_id}] No baseline scenes")
    return(list())
  }
  if (is.null(p_items) || length(p_items) == 0) {
    log_warn("[tile {tile_id}] No post-fire scenes")
    return(list())
  }

  # Build baseline
  baseline_path <- tryCatch(
    build_tile_baseline(b_items, CFG$baseline_n_img, clip_aoi_wkt,
                        target_crs, token, baseline_dir, run_tag, tile_id),
    error = function(e) { log_error("[tile {tile_id}] baseline failed: {e$message}"); NULL }
  )
  if (is.null(baseline_path)) return(list())

  # Process post-fire dates in chronological order
  p_items_sorted <- p_items[order(vapply(p_items, function(x) x$date, as.Date(NA)))]
  prev_date      <- NULL
  outputs        <- list()

  tally <- c(written = 0L, cached = 0L, too_cloudy = 0L,
             skipped_gap = 0L, stream_fail = 0L,
             mask_miss = 0L, mask_empty = 0L, error = 0L)

  for (item in p_items_sorted) {
    # FIX B3 continued: refresh token before each acquisition.
    # Cheap when cached (60s safety margin); only re-fetches near expiry.
    token <- refresh_token()

    r <- tryCatch(
      process_postfire_date(item, baseline_path, mask_path,
                            clip_aoi_wkt, target_crs, dnbr_dir,
                            run_tag, tile_id, prev_date, token),
      error = function(e) {
        log_error("  [tile {tile_id} {item$date}] {e$message}")
        list(status = "error", path = NULL, date = item$date, observed = FALSE)
      }
    )

    tally[r$status] <- tally[r$status] + 1L

    if (!is.null(r$path)) {
      outputs[[length(outputs) + 1]] <- list(
        path = r$path, date = item$date, tile_id = tile_id
      )
    }

    # FIX 2026-09: advance the gap anchor on any OBSERVED date, not only on
    # dates that produced a dNBR. The old code advanced prev_date only when a
    # path came back, so a single failure froze the anchor and every later
    # date failed the gap test against it — the series ended permanently.
    if (isTRUE(r$observed)) prev_date <- r$date
  }

  log_info("[tile {tile_id}] {length(p_items_sorted)} scene(s): ",
           "written={tally[['written']]} cached={tally[['cached']]} ",
           "cloudy={tally[['too_cloudy']]} gap={tally[['skipped_gap']]} ",
           "streamfail={tally[['stream_fail']]} maskmiss={tally[['mask_miss']]} ",
           "maskempty={tally[['mask_empty']]} error={tally[['error']]}")

  attr(outputs, "tally") <- tally
  outputs
}

# ── Entrypoint ────────────────────────────────────────────────────────────────

run_compute_dnbr <- function(root_dir, cfg = NULL) {
  source(file.path(root_dir, "R", "config.R"))
  source(file.path(root_dir, "R", "00_gcs_utils.R"))
  terraOptions(memmax = CFG$terra_mem_gb,
               tempdir = file.path(root_dir, CFG$dir_tmp),
               todisk  = CFG$terra_todisk,
               progress = 0)

  dir_create(c(file.path(root_dir, CFG$dir_logs),
                file.path(root_dir, CFG$dir_dnbr),
                file.path(root_dir, CFG$dir_baselines),
                file.path(root_dir, CFG$dir_tmp)))

  log_appender(appender_tee(
    file.path(root_dir, CFG$dir_logs, paste0(CFG$run_tag, "_02_dnbr.log"))
  ))
  log_threshold(INFO)
  log_info("=== Step 02: Stream S2 + compute dNBR (parallel) | {CFG$run_id} ===")

  mask_path <- file.path(root_dir, CFG$dir_cropland_mask,
                         paste0(CFG$run_tag, "_cropland_20m.tif"))
  if (!file.exists(mask_path))
    stop("Cropland mask not found: ", mask_path, ". Run Step 01 first.")

  # FIX A5: dead `aoi_sf <- NULL` conditional removed. Load directly.
  shp_path <- file.path(root_dir, CFG$shapefile_path, CFG$gpkg_file)
  aoi_sf   <- st_read(shp_path, layer = CFG$gpkg_lyr_dist, quiet = TRUE) |>
              st_make_valid() |> st_union() |> st_buffer(CFG$aoi_buffer_deg)

  # FIX 2026-09: carry the AOI as a 4326 polygon (WKT, so it crosses the
  # furrr process boundary as plain text). Each tile derives its own clip
  # window from this in its own native CRS — see stream_band_clipped().
  aoi_ll       <- st_transform(aoi_sf, 4326)
  clip_aoi_wkt <- st_as_text(st_geometry(aoi_ll)[[1]])
  bbox_wgs84   <- as.numeric(st_bbox(aoi_ll))

  # Get an initial CDSE token in the master process for STAC searches.
  # Workers will fetch their own tokens when they start.
  # FIX 2026-09: a missing CDSE credential is fatal, not a warning. The old
  # code warned, returned NULL, and proceeded to make unauthenticated
  # requests — which surfaced as an unrelated HTTP 400 about response size
  # rather than "you have no token".
  token <- if (CFG$stac_source == "CDSE") {
    tk <- tryCatch(get_cdse_token(), error = function(e) { log_error(e$message); NULL })
    if (is.null(tk))
      stop("CDSE selected but no token could be obtained. Set CDSE_CLIENT_ID ",
           "and CDSE_CLIENT_SECRET in ~/.Renviron, or run with --stac=MPC.")
    tk
  } else NULL
  set_gdal_streaming_env(token)

  # ── STAC search for both windows ──────────────────────────────────────────
  log_info("Searching STAC: baseline window...")
  log_info(paste("bbox_wgs84:", paste(round(bbox_wgs84,3), collapse=", ")))
  base_features <- stac_search_window(bbox_wgs84,
                    CFG$baseline_start, CFG$baseline_end, CFG$stac_source, token)
  log_info("Searching STAC: post-fire window...")
  post_features <- stac_search_window(bbox_wgs84,
                    CFG$season_start, CFG$season_end, CFG$stac_source, token)

  # Parse and group by tile
  parse_and_group <- function(features) {
    meta <- lapply(features, function(it) {
      m <- parse_item_meta(it, CFG$stac_source)
      m$item <- it
      m
    })
    # Deduplicate: keep lowest cloud per (tile, date)
    meta_df <- bind_rows(lapply(seq_along(meta), function(i) {
      m <- meta[[i]]
      data.frame(tile_id = m$tile_id, date = as.character(m$date),
                 cloud = m$cloud, idx = i, stringsAsFactors = FALSE)
    }))
    meta_df <- meta_df[order(meta_df$tile_id, meta_df$date, meta_df$cloud), ]
    meta_df <- meta_df[!duplicated(meta_df[, c("tile_id","date")]), ]
    grouped <- split(meta[meta_df$idx], meta_df$tile_id)
    grouped
  }

  log_info("Baseline features found: {length(base_features)}")
  log_info("Post-fire features found: {length(post_features)}")
  if (length(base_features) == 0) stop("No baseline scenes found. Check dates/bbox/CDSE credentials.")
  if (length(post_features) == 0) stop("No post-fire scenes found. Check dates/bbox/CDSE credentials.")
  baseline_by_tile <- parse_and_group(base_features)
  postfire_by_tile <- parse_and_group(post_features)
  all_tiles        <- unique(c(names(baseline_by_tile), names(postfire_by_tile)))
  log_info("{length(all_tiles)} MGRS tile(s): {paste(all_tiles, collapse=', ')}")

  # ── Parallel processing over tiles ────────────────────────────────────────
  log_info("Starting parallel tile processing (workers: {CFG$n_workers})...")
  plan(multisession, workers = CFG$n_workers)
  on.exit(plan(sequential), add = TRUE)

  results_nested <- future_map(
    all_tiles,
    ~process_tile(
      tile_id        = .x,
      baseline_items = baseline_by_tile,
      postfire_items = postfire_by_tile,
      clip_aoi_wkt   = clip_aoi_wkt,
      target_crs     = CFG$target_crs,
      mask_path      = mask_path,
      dnbr_dir       = file.path(root_dir, CFG$dir_dnbr),
      baseline_dir   = file.path(root_dir, CFG$dir_baselines),
      run_tag        = CFG$run_tag,
      cfg            = CFG,
      tile_log_dir   = file.path(root_dir, CFG$dir_logs, "step02_tiles")
    ),
    .options = furrr_options(seed = TRUE, packages = c("terra","rstac","sf",
                                                        "lubridate","glue","logger","fs"))
  )

  all_outputs <- unlist(results_nested, recursive = FALSE)

  # FIX 2026-09: report per-tile scene accounting up front. Tiles that
  # produced far fewer dNBRs than they had scenes are the signature of the
  # failure modes fixed in this commit; surfacing them beats inferring the
  # problem later from raster dimensions.
  tallies <- do.call(rbind, lapply(seq_along(all_tiles), function(i) {
    tl <- attr(results_nested[[i]], "tally")
    if (is.null(tl)) return(NULL)
    data.frame(tile_id = all_tiles[i],
               scenes  = length(postfire_by_tile[[all_tiles[i]]]) %||% 0L,
               t(as.data.frame(tl)), row.names = NULL)
  }))

  if (!is.null(tallies)) {
    tallies$used_pct <- round(100 * (tallies$written + tallies$cached) /
                              pmax(tallies$scenes, 1L))
    thin <- tallies[tallies$used_pct < 50, ]
    log_info("Scene usage: median {stats::median(tallies$used_pct)}% across {nrow(tallies)} tiles")
    if (nrow(thin) > 0) {
      log_warn("{nrow(thin)} tile(s) used under 50% of available scenes:")
      for (j in seq_len(nrow(thin)))
        log_warn("  {thin$tile_id[j]}: {thin$written[j]+thin$cached[j]}/{thin$scenes[j]} ({thin$used_pct[j]}%)")
    }
    utils::write.csv(tallies,
      file.path(root_dir, CFG$dir_logs,
                paste0(CFG$run_tag, "_02_tile_scene_usage.csv")),
      row.names = FALSE)
  }

  log_info("Step 02 complete. {length(all_outputs)} dNBR tiles produced.")
  invisible(all_outputs)
}

if (!interactive() && identical(commandArgs(TRUE)[1], "--step=02")) {
  root_dir <- normalizePath(Sys.getenv("PIPELINE_ROOT", "."))
  run_compute_dnbr(root_dir)
}
