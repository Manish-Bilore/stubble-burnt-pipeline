# =============================================================================
# config.R  —  Config BUILDER (v5)
# =============================================================================
# DO NOT hard-code run parameters here.
# All run-time values come from CLI args parsed in run_pipeline.R and passed
# to build_config().  This file only defines:
#   - build_config()          construct CFG from named parameters
#   - GDAL streaming setup    set_gdal_streaming_env()
#
# Credentials are sourced from credentials/CDSE_api.R and
# credentials/earthdata_api.R by run_pipeline.R.
#
# GCS helpers (gcs_push, gcs_upload_file, etc.) live in 00_gcs_utils.R
# and are sourced by run_pipeline.R.
# =============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(lubridate)
})

#' Build the global CFG list from run-time parameters.
#' Called by run_pipeline.R after parsing CLI args.
build_config <- function(
    state,
    year            = as.integer(format(Sys.Date(), "%Y")),
    season          = "Rabi",
    season_start    = NULL,
    season_end      = NULL,
    baseline_start  = NULL,
    baseline_end    = NULL,
    nrt             = FALSE,
    nrt_lookback    = 30L,
    nrt_baseline    = 45L,
    stac_source     = "CDSE",
    max_cloud_pct   = 70L,
    dnbr_burn_min   = 0.10,
    n_workers       = 4L,
    gcs_bucket      = "",
    root_dir        = normalizePath(Sys.getenv("PIPELINE_ROOT", "."))
) {
  source(file.path(root_dir, "R", "state_registry.R"))
  state_info <- get_state_info(state)

  # ── Resolve date windows ─────────────────────────────────────────────────
  if (nrt) {
    dates <- nrt_date_windows(nrt_lookback, nrt_baseline)
    season <- "NRT"
    year   <- as.integer(format(Sys.Date(), "%Y"))
    message(sprintf("[config] NRT mode: post-fire %s → %s | baseline %s → %s",
                    dates$season_start, dates$season_end,
                    dates$baseline_start, dates$baseline_end))
  } else if (!is.null(season_start) && !is.null(season_end) &&
             !is.null(baseline_start) && !is.null(baseline_end)) {
    dates <- list(season_start   = season_start,
                  season_end     = season_end,
                  baseline_start = baseline_start,
                  baseline_end   = baseline_end)
  } else {
    dates <- default_season_dates(state_info, season, year)
  }

  # Validate dates
  for (d in c("season_start","season_end","baseline_start","baseline_end")) {
    parsed <- suppressWarnings(as.Date(dates[[d]]))
    if (is.na(parsed)) stop("Invalid date for '", d, "': ", dates[[d]])
  }
  if (as.Date(dates$baseline_end) >= as.Date(dates$season_start))
    stop("baseline_end (", dates$baseline_end,
         ") must be before season_start (", dates$season_start, ")")

  CFG <- list(
    # identity
    state           = state_info$state_code,
    display_name    = state_info$display_name,
    season          = season,
    year            = as.integer(year),

    # dates
    season_start    = dates$season_start,
    season_end      = dates$season_end,
    baseline_start  = dates$baseline_start,
    baseline_end    = dates$baseline_end,

    # STAC
    stac_source         = toupper(stac_source),
    cdse_stac_url       = "https://stac.dataspace.copernicus.eu/v1",
    cdse_collection     = "sentinel-2-l2a",
    cdse_s3_endpoint    = "https://eodata.dataspace.copernicus.eu",
    mpc_stac_url        = "https://planetarycomputer.microsoft.com/api/stac/v1",
    mpc_collection      = "sentinel-2-l2a",
    s2_bands            = c("B8A", "B12"),
    s2_scl_band         = "SCL",
    max_cloud_pct       = as.integer(max_cloud_pct),

    # GDAL HTTP tuning
    gdal_http_max_retry   = 3L,
    gdal_http_retry_delay = 5L,
    gdal_merge_ranges     = TRUE,
    gdal_multirange       = TRUE,

    # compositing
    composite_days  = 5L,
    baseline_n_img  = 3L,
    max_gap_days    = 8L,
    min_valid_frac  = 0.30,

    # burn thresholds
    dnbr_burn_min   = as.numeric(dnbr_burn_min),
    severity_breaks = c(0.00, 0.10, 0.27, 0.44, 0.66, 2.00),
    severity_labels = c("Unburned", "Low", "Moderate-Low", "Moderate-High", "High"),

    # GFSAD
    gfsad_product   = "GFSAD30SAAFGIRCE",
    gfsad_crop_vals = c(2L),

    # spatial
    target_crs      = state_info$utm_crs,
    native_res_m    = 20L,
    pixel_area_ha   = (20 * 20) / 10000,

    # ── GeoPackage references (FIX B2) ──────────────────────────────────────
    # These now point to real fields in state_registry.R. The v4 names
    # `shp_districts` and `shp_state` did not exist in the registry, so
    # CFG$shp_districts and CFG$shp_state were silently NULL.
    gpkg_file       = state_info$gpkg_file,
    gpkg_lyr_dist   = state_info$gpkg_lyr_dist,
    gpkg_lyr_state  = state_info$gpkg_lyr_state,
    dist_col        = state_info$dist_col,

    # GCS
    gcs_bucket      = gcs_bucket,
    gcs_prefix      = "pipeline",

    # paths
    dir_raw_gfsad   = "data/raw/gfsad",
    dir_dnbr        = "data/interim/dnbr",
    dir_baselines   = "data/interim/baselines",
    dir_out_tif     = "data/outputs/geotiff",
    dir_out_csv     = "data/outputs/csv",
    dir_logs        = "data/outputs/logs",
    dir_tmp         = "data/tmp",
    shapefile_path  = "data/raw/shapefiles",

    # compute
    n_workers       = as.integer(n_workers),
    terra_mem_gb    = 10,
    terra_todisk    = TRUE,
    duckdb_threads  = as.integer(n_workers),
    aoi_buffer_deg  = 0.05,

    # runtime
    root_dir        = root_dir
  )

  # Derived tags — include date range so parallel runs don't collide
  CFG$run_id  <- sprintf("%s_%s_%s_%s_to_%s",
                          CFG$state, tolower(CFG$season), CFG$year,
                          gsub("-", "", CFG$season_start),
                          gsub("-", "", CFG$season_end))
  CFG$run_tag <- CFG$run_id

  message(sprintf("[config] %s | %s %s | post-fire: %s → %s | baseline: %s → %s | STAC: %s",
                  CFG$display_name, CFG$season, CFG$year,
                  CFG$season_start, CFG$season_end,
                  CFG$baseline_start, CFG$baseline_end,
                  CFG$stac_source))
  CFG
}

#' Set GDAL env vars for efficient COG HTTP range requests.
set_gdal_streaming_env <- function(bearer_token = NULL, cfg = NULL) {
  retry <- if (!is.null(cfg)) cfg$gdal_http_max_retry   else 3L
  delay <- if (!is.null(cfg)) cfg$gdal_http_retry_delay else 5L
  merge <- if (!is.null(cfg)) cfg$gdal_merge_ranges     else TRUE
  multi <- if (!is.null(cfg)) cfg$gdal_multirange       else TRUE

  Sys.setenv(GDAL_HTTP_MAX_RETRY                  = as.character(retry))
  Sys.setenv(GDAL_HTTP_RETRY_DELAY                = as.character(delay))
  Sys.setenv(GDAL_HTTP_MERGE_CONSECUTIVE_RANGES   = if (merge) "YES" else "NO")
  Sys.setenv(GDAL_HTTP_MULTIRANGE                 = if (multi) "YES" else "NO")
  Sys.setenv(VSI_CACHE                            = "TRUE")
  Sys.setenv(VSI_CACHE_SIZE                       = "100000000")  # 100 MB HTTP cache
  if (!is.null(bearer_token) && nchar(bearer_token) > 0)
    Sys.setenv(GDAL_HTTP_HEADERS = paste0("Authorization: Bearer ", bearer_token))
  invisible(NULL)
}

# NOTE: gcs_push() lives in R/00_gcs_utils.R and is sourced by run_pipeline.R.
# It was duplicated here in v4; consolidating to a single source-of-truth.
