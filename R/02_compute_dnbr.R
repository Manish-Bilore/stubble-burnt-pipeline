# =============================================================================
# 02_compute_dnbr.R
# Stream Sentinel-2 COGs directly from STAC via /vsicurl/ HTTP range requests.
# No full-tile downloads. Clips to AOI bbox before loading pixels.
# Computes dNBR AND applies GFSAD crop mask in a single raster pass per tile/date.
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
      stac_search(collections = CFG$cdse_collection, bbox = bbox,
                  datetime = dt_range, limit = 500) |>
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

#' Load a single band from a COG URL, clipped to the AOI bbox window.
#' Uses /vsicurl/ — GDAL only fetches the required HTTP byte ranges.
stream_band_clipped <- function(url, clip_bbox_utm, target_crs,
                                 scale = 1e-4, bearer_token = NULL) {
  if (!is.null(bearer_token))
    Sys.setenv(GDAL_HTTP_HEADERS = paste0("Authorization: Bearer ", bearer_token))

  r <- tryCatch(
    rast(paste0("/vsicurl/", url)),
    error = function(e) { message("  [stream fail] ", url, ": ", e$message); NULL }
  )
  if (is.null(r)) return(NULL)

  # Reproject clip bbox to raster's native CRS (S2 tiles are in their UTM zone)
  clip_vect      <- vect(st_as_sf(st_sfc(
    st_as_sfc(st_bbox(clip_bbox_utm)), crs = target_crs
  )))
  clip_native    <- project(clip_vect, crs(r))

  r_clip <- tryCatch(
    crop(r, clip_native),
    error = function(e) { message("  [crop fail] ", e$message); NULL }
  )
  if (is.null(r_clip)) return(NULL)

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
build_tile_baseline <- function(tile_items, n_img, clip_bbox_utm, target_crs,
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
                                clip_bbox_utm, target_crs, bearer_token = token)
    b12 <- stream_band_clipped(get_asset_url(raw_it, "B12", CFG$stac_source),
                                clip_bbox_utm, target_crs, bearer_token = token)
    scl <- stream_band_clipped(get_asset_url(raw_it, "SCL", CFG$stac_source),
                                clip_bbox_utm, target_crs,
                                scale = 1, bearer_token = token)
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

# ── Per-date dNBR + mask (single pass) ───────────────────────────────────────

#' Stream one post-fire S2 acquisition, compute dNBR, apply GFSAD mask,
#' classify severity — all in a single raster pass before writing to disk.
process_postfire_date <- function(item, baseline_path, mask_path,
                                   clip_bbox_utm, target_crs, out_dir,
                                   run_tag, tile_id, prev_date = NULL,
                                   token = NULL) {
  acq_date <- item$date
  date_str <- format(acq_date, "%Y%m%d")
  out_path <- file.path(out_dir,
    glue("{run_tag}_{date_str}_{tile_id}_dnbr.tif"))

  if (file.exists(out_path)) {
    log_debug("  [skip] {basename(out_path)}")
    return(out_path)
  }

  # Gap check
  if (!is.null(prev_date)) {
    gap <- as.integer(acq_date - prev_date)
    if (gap > CFG$max_gap_days) {
      log_warn("  {date_str} tile {tile_id}: gap {gap}d > {CFG$max_gap_days}d — skipped")
      return(NULL)
    }
  }

  raw_item <- sign_item(item$item, CFG$stac_source)

  scl <- stream_band_clipped(get_asset_url(raw_item, "SCL", CFG$stac_source),
                              clip_bbox_utm, target_crs,
                              scale = 1, bearer_token = token)
  if (is.null(scl)) return(NULL)

  vfrac <- valid_frac_from_scl(scl)
  if (vfrac < CFG$min_valid_frac) {
    log_warn("  {date_str} tile {tile_id}: valid_frac={round(vfrac,2)} — too cloudy")
    return(NULL)
  }

  b8a <- stream_band_clipped(get_asset_url(raw_item, "B8A", CFG$stac_source),
                              clip_bbox_utm, target_crs, bearer_token = token)
  b12 <- stream_band_clipped(get_asset_url(raw_item, "B12", CFG$stac_source),
                              clip_bbox_utm, target_crs, bearer_token = token)
  if (is.null(b8a) || is.null(b12)) return(NULL)

  b12 <- resample(b12, b8a, method = "bilinear")
  scl <- resample(scl, b8a, method = "near")

  baseline <- rast(baseline_path)
  if (!compareGeom(baseline, b8a, stopOnError = FALSE))
    baseline <- resample(baseline, b8a, method = "bilinear")

  nbr_post <- compute_nbr(b8a, b12)
  nbr_post[!(scl %in% SCL_VALID)] <- NA

  dnbr     <- baseline - nbr_post
  severity <- classify_severity(dnbr)

  # ── Apply GFSAD crop mask in same pass ──────────────────────────────────────
  crop_mask <- rast(mask_path)
  if (!compareGeom(crop_mask, dnbr, stopOnError = FALSE))
    crop_mask <- resample(crop_mask, dnbr, method = "near")
  crop_mask <- crop(crop_mask, dnbr)

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
  out_path
}

# ── Tile-level worker (runs inside furrr future) ──────────────────────────────

process_tile <- function(tile_id, baseline_items, postfire_items,
                          clip_bbox_utm, target_crs, mask_path,
                          dnbr_dir, baseline_dir, run_tag, cfg) {

  # Re-source config and credentials inside worker (futures have clean envs)
  CFG <<- cfg
  source(file.path(Sys.getenv("PIPELINE_ROOT"), "R", "config.R"))
  source(file.path(Sys.getenv("PIPELINE_ROOT"), "R", "00_gcs_utils.R"))
  source(file.path(Sys.getenv("PIPELINE_ROOT"), "credentials", "CDSE_api.R"))

  suppressPackageStartupMessages({
    library(terra); library(rstac); library(sf)
    library(lubridate); library(glue); library(logger); library(fs)
  })
  terraOptions(memmax = CFG$terra_mem_gb, tempdir = file.path(
    Sys.getenv("PIPELINE_ROOT"), CFG$dir_tmp), todisk = CFG$terra_todisk,
    progress = 0)

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
    build_tile_baseline(b_items, CFG$baseline_n_img, clip_bbox_utm,
                        target_crs, token, baseline_dir, run_tag, tile_id),
    error = function(e) { log_error("[tile {tile_id}] baseline failed: {e$message}"); NULL }
  )
  if (is.null(baseline_path)) return(list())

  # Process post-fire dates in chronological order
  p_items_sorted <- p_items[order(vapply(p_items, function(x) x$date, as.Date(NA)))]
  prev_date      <- NULL
  outputs        <- list()

  for (item in p_items_sorted) {
    # FIX B3 continued: refresh token before each acquisition.
    # Cheap when cached (60s safety margin); only re-fetches near expiry.
    token <- refresh_token()

    path <- tryCatch(
      process_postfire_date(item, baseline_path, mask_path,
                            clip_bbox_utm, target_crs, dnbr_dir,
                            run_tag, tile_id, prev_date, token),
      error = function(e) { log_error("  [tile {tile_id} {item$date}] {e$message}"); NULL }
    )
    if (!is.null(path)) {
      outputs[[length(outputs) + 1]] <- list(
        path = path, date = item$date, tile_id = tile_id
      )
      prev_date <- item$date
    }
  }
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

  mask_path <- file.path(root_dir, CFG$dir_raw_gfsad,
                         paste0(CFG$run_tag, "_gfsad30_20m.tif"))
  if (!file.exists(mask_path))
    stop("GFSAD mask not found: ", mask_path, ". Run Step 01 first.")

  # FIX A5: dead `aoi_sf <- NULL` conditional removed. Load directly.
  shp_path <- file.path(root_dir, CFG$shapefile_path, CFG$gpkg_file)
  aoi_sf   <- st_read(shp_path, layer = CFG$gpkg_lyr_dist, quiet = TRUE) |>
              st_make_valid() |> st_union() |> st_buffer(CFG$aoi_buffer_deg)

  aoi_utm       <- st_transform(aoi_sf, CFG$target_crs)
  clip_bbox_utm <- st_bbox(aoi_utm)
  bbox_wgs84    <- as.numeric(st_bbox(st_transform(aoi_sf, 4326)))

  # Get an initial CDSE token in the master process for STAC searches.
  # Workers will fetch their own tokens when they start.
  token <- if (CFG$stac_source == "CDSE") {
    tryCatch(get_cdse_token(), error = function(e) { log_warn(e$message); NULL })
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
    meta_df <- bind_rows(lapply(meta, function(m) {
      data.frame(tile_id=m$tile_id, date=m$date, cloud=m$cloud, idx=1, stringsAsFactors=FALSE)
    }))
    meta_df <- meta_df[order(meta_df$tile_id, meta_df$date, meta_df$cloud), ]
    meta_df <- meta_df[!duplicated(meta_df[,c("tile_id","date")]), ]

    grouped <- split(
      lapply(seq_len(nrow(meta_df)), function(i) {
        orig <- meta[[which(sapply(meta, function(m) m$tile_id == meta_df$tile_id[i] &
                                                       m$date == meta_df$date[i]))[[1]]]]
        orig
      }),
      meta_df$tile_id
    )
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
      clip_bbox_utm  = clip_bbox_utm,
      target_crs     = CFG$target_crs,
      mask_path      = mask_path,
      dnbr_dir       = file.path(root_dir, CFG$dir_dnbr),
      baseline_dir   = file.path(root_dir, CFG$dir_baselines),
      run_tag        = CFG$run_tag,
      cfg            = CFG
    ),
    .options = furrr_options(seed = TRUE, packages = c("terra","rstac","sf",
                                                        "lubridate","glue","logger","fs"))
  )

  all_outputs <- unlist(results_nested, recursive = FALSE)
  log_info("Step 02 complete. {length(all_outputs)} dNBR tiles produced.")
  invisible(all_outputs)
}

if (!interactive() && identical(commandArgs(TRUE)[1], "--step=02")) {
  root_dir <- normalizePath(Sys.getenv("PIPELINE_ROOT", "."))
  run_compute_dnbr(root_dir)
}
