# =============================================================================
# 04_summarise.R  (v5.1 — cumulative-products methodology)
# =============================================================================
# Computes season-level burn statistics from Step 03's per-district cumulative
# products. Each pixel contributes exactly once to season totals.
#
# WHY THIS CHANGED FROM v5.0:
#   The original version summed per-date burn detections across the season,
#   which double-counted pixels visible as burned on multiple dates. Punjab
#   Rabi 2024 results were inflated ~10-100x (Patiala reported 4.28M ha vs
#   its true total area of 388k ha; state total reported 32M ha vs Punjab's
#   total area of 5M ha).
#
# NEW METHODOLOGY:
#   - District summary  ->  pixels where max_dnbr >= threshold (unique pixels)
#   - 5-day timeseries  ->  pixels binned by first_burn_doy (each pixel once,
#                           in the 5-day window of its first detection)
#   - Severity          ->  max_dnbr classified into severity tiers
#
# Outputs:
#   data/outputs/csv/<run_tag>_district_summary.csv     (one row per district)
#   data/outputs/csv/<run_tag>_timeseries_5day.csv      (district x 5-day bin)
#   data/outputs/csv/<run_tag>_severity_breakdown.csv   (district x severity)
#   data/outputs/csv/<run_tag>_analytics.duckdb         (same 3 tables; for SQL)
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(exactextractr)
  library(dplyr)
  library(lubridate)
  library(glue)
  library(fs)
  library(logger)
  library(DBI)
  library(duckdb)
})

# =============================================================================
# extract_district()
# Reads the three cumulative rasters for one district, runs exact_extract once
# on the stack so pixel rows align across bands, and computes:
#   - season-level summary row
#   - per-severity-tier breakdown
#   - 5-day first-detection timeseries
# Returns list(summary, severity, timeseries), or NULL if no burn.
# =============================================================================

extract_district <- function(district_name, district_sf, cfg, out_tif_dir) {
  safe    <- gsub("[^A-Za-z0-9_]", "_", toupper(district_name))
  run_tag <- cfg$run_tag

  max_tif <- file.path(out_tif_dir, glue("{run_tag}_{safe}_max_dnbr.tif"))
  doy_tif <- file.path(out_tif_dir, glue("{run_tag}_{safe}_first_burn_doy.tif"))
  cnt_tif <- file.path(out_tif_dir, glue("{run_tag}_{safe}_cumulative_count.tif"))

  # Step 03 only writes cumulative products for districts with at least one
  # burn detection. Missing files = district had no burn.
  if (!all(file.exists(c(max_tif, doy_tif, cnt_tif)))) {
    log_info("  {district_name}: no cumulative products (no burn detected)")
    return(NULL)
  }

  # Stack as multi-band raster so exact_extract returns aligned per-pixel rows
  stack_r <- rast(c(max_tif, doy_tif, cnt_tif))
  names(stack_r) <- c("max_dnbr", "first_doy", "cum_count")

  dist_proj <- sf::st_transform(district_sf, crs(stack_r))
  ex <- exact_extract(stack_r, dist_proj, fun = NULL, progress = FALSE)[[1]]

  # Filter to burned pixels: max_dnbr present AND >= threshold
  burned <- ex[!is.na(ex$max_dnbr) & ex$max_dnbr >= cfg$dnbr_burn_min, ]
  if (nrow(burned) == 0) {
    log_info("  {district_name}: 0 burned pixels above threshold")
    return(NULL)
  }

  # Coverage-weighted area per pixel (partial pixels at district boundary
  # contribute fractionally -- standard exactextractr behaviour)
  burned$area_ha <- cfg$pixel_area_ha * burned$coverage_fraction

  total_area  <- sum(burned$area_ha)
  doy_to_date <- function(d) as.Date(d - 1, origin = paste0(cfg$year, "-01-01"))

  # -- District summary row ---------------------------------------------------
  district_total <- data.frame(
    district              = district_name,
    burn_area_ha          = round(total_area, 2),
    burn_pixel_count      = nrow(burned),
    mean_dNBR             = round(weighted.mean(burned$max_dnbr,
                                                burned$coverage_fraction), 4),
    max_dNBR              = round(max(burned$max_dnbr), 4),
    first_detection       = doy_to_date(min(burned$first_doy, na.rm = TRUE)),
    last_first_detection  = doy_to_date(max(burned$first_doy, na.rm = TRUE)),
    mean_persistence_days = round(weighted.mean(burned$cum_count,
                                                 burned$coverage_fraction) *
                                  cfg$composite_days, 1),
    season_start          = as.Date(cfg$season_start),
    season_end            = as.Date(cfg$season_end),
    stringsAsFactors      = FALSE
  )

  # -- Severity breakdown -----------------------------------------------------
  burned$severity_class <- as.character(cut(
    burned$max_dnbr,
    breaks         = cfg$severity_breaks,
    labels         = cfg$severity_labels,
    include.lowest = TRUE,
    right          = FALSE
  ))

  sev_df <- burned |>
    group_by(severity_class) |>
    summarise(
      burn_area_ha = round(sum(area_ha), 2),
      pixel_count  = n(),
      .groups      = "drop"
    ) |>
    mutate(
      district              = district_name,
      pct_of_district_total = round(100 * burn_area_ha / total_area, 1)
    ) |>
    select(district, severity_class, burn_area_ha, pixel_count,
           pct_of_district_total)

  # -- 5-day timeseries (first-detection methodology) -------------------------
  season_start_doy <- yday(as.Date(cfg$season_start))
  ts_burned        <- burned[!is.na(burned$first_doy), ]
  ts_burned$bin_index     <- floor((ts_burned$first_doy - season_start_doy) /
                                   cfg$composite_days)
  ts_burned$bin_start_doy <- season_start_doy +
                             ts_burned$bin_index * cfg$composite_days

  ts_df <- ts_burned |>
    group_by(bin_start_doy) |>
    summarise(
      burn_area_ha     = round(sum(area_ha), 2),
      burn_pixel_count = n(),
      mean_dNBR        = round(weighted.mean(max_dnbr, coverage_fraction), 4),
      max_dNBR         = round(max(max_dnbr), 4),
      .groups          = "drop"
    ) |>
    mutate(
      district       = district_name,
      bin_start_date = doy_to_date(bin_start_doy),
      bin_end_date   = bin_start_date + cfg$composite_days - 1
    ) |>
    select(district, bin_start_date, bin_end_date, bin_start_doy,
           burn_area_ha, burn_pixel_count, mean_dNBR, max_dNBR)

  log_info("  {district_name}: {nrow(burned)} burned px, {round(total_area, 0)} ha")
  list(summary = district_total, severity = sev_df, timeseries = ts_df)
}

# =============================================================================
# write_duckdb_analytics()
# Writes the same three aggregated tables into a .duckdb file for ad-hoc SQL.
# This is a convenience side-output -- all data is also in the CSVs.
# =============================================================================

write_duckdb_analytics <- function(duck_path, district_summary, severity_df,
                                    timeseries_df, duckdb_threads = 4) {
  if (file.exists(duck_path)) file.remove(duck_path)

  con <- dbConnect(duckdb::duckdb(), dbdir = duck_path, read_only = FALSE)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(con, glue("PRAGMA threads = {duckdb_threads}"))

  dbWriteTable(con, "district_summary", district_summary, overwrite = TRUE)
  dbWriteTable(con, "severity",         severity_df,      overwrite = TRUE)
  dbWriteTable(con, "timeseries",       timeseries_df,    overwrite = TRUE)

  # Helpful indices for the common query patterns
  dbExecute(con, "CREATE INDEX idx_sev_district ON severity(district)")
  dbExecute(con, "CREATE INDEX idx_ts_district  ON timeseries(district)")
  dbExecute(con, "CREATE INDEX idx_ts_date      ON timeseries(bin_start_date)")

  invisible(duck_path)
}

# =============================================================================
# run_summarise()  --  orchestrator entry point
# =============================================================================

run_summarise <- function(root_dir, cfg = NULL) {
  source(file.path(root_dir, "R", "config.R"))
  source(file.path(root_dir, "R", "00_gcs_utils.R"))

  log_appender(appender_tee(
    file.path(root_dir, CFG$dir_logs,
              paste0(CFG$run_tag, "_04_summarise.log"))
  ))
  log_threshold(INFO)
  log_info("=== Step 04: Cumulative-product summaries | {CFG$run_id} ===")

  out_tif_dir <- file.path(root_dir, CFG$dir_out_tif)
  out_csv_dir <- file.path(root_dir, CFG$dir_out_csv)
  dir_create(out_csv_dir)

  # Load district polygons
  shp_path <- file.path(root_dir, CFG$shapefile_path, CFG$gpkg_file)
  districts_sf <- sf::st_read(shp_path, layer = CFG$gpkg_lyr_dist,
                              quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)

  dist_col <- CFG$dist_col
  if (!dist_col %in% names(districts_sf)) {
    dist_col <- intersect(c("DISTRICT", "district", "District", "NAME", "name"),
                          names(districts_sf))[1]
    if (is.na(dist_col))
      stop("Cannot find district name column in ", shp_path)
    log_warn("CFG$dist_col '{CFG$dist_col}' not found; using fallback '{dist_col}'")
  }
  district_names <- unique(districts_sf[[dist_col]])
  log_info("Processing {length(district_names)} districts...")

  # Extract per-district
  results <- lapply(district_names, function(dn) {
    dist_sf <- districts_sf[districts_sf[[dist_col]] == dn, ]
    tryCatch(
      extract_district(dn, dist_sf, CFG, out_tif_dir),
      error = function(e) {
        log_error("  [{dn}] {conditionMessage(e)}")
        NULL
      }
    )
  })

  with_burn  <- Filter(Negate(is.null), results)
  no_burn    <- setdiff(district_names,
                        vapply(with_burn, function(r) r$summary$district,
                               character(1)))

  if (length(with_burn) == 0) {
    log_warn("No districts had any burn detections.")
    return(invisible(NULL))
  }

  district_summary <- bind_rows(lapply(with_burn, `[[`, "summary")) |>
                      arrange(desc(burn_area_ha))
  severity_df      <- bind_rows(lapply(with_burn, `[[`, "severity")) |>
                      arrange(district, severity_class)
  timeseries_df    <- bind_rows(lapply(with_burn, `[[`, "timeseries")) |>
                      arrange(district, bin_start_date)

  # Add zero-rows for districts with no burn
  if (length(no_burn) > 0) {
    zero_rows <- data.frame(
      district              = no_burn,
      burn_area_ha          = 0,
      burn_pixel_count      = 0L,
      mean_dNBR             = NA_real_,
      max_dNBR              = NA_real_,
      first_detection       = as.Date(NA),
      last_first_detection  = as.Date(NA),
      mean_persistence_days = NA_real_,
      season_start          = as.Date(CFG$season_start),
      season_end            = as.Date(CFG$season_end),
      stringsAsFactors      = FALSE
    )
    district_summary <- bind_rows(district_summary, zero_rows)
  }

  # -- Write CSVs --------------------------------------------------------------
  dist_csv <- file.path(out_csv_dir, paste0(CFG$run_tag, "_district_summary.csv"))
  ts_csv   <- file.path(out_csv_dir, paste0(CFG$run_tag, "_timeseries_5day.csv"))
  sev_csv  <- file.path(out_csv_dir, paste0(CFG$run_tag, "_severity_breakdown.csv"))

  write.csv(district_summary, dist_csv, row.names = FALSE)
  write.csv(timeseries_df,    ts_csv,   row.names = FALSE)
  write.csv(severity_df,      sev_csv,  row.names = FALSE)

  log_info("District summary  -> {basename(dist_csv)} ({nrow(district_summary)} rows)")
  log_info("Timeseries        -> {basename(ts_csv)} ({nrow(timeseries_df)} rows)")
  log_info("Severity          -> {basename(sev_csv)} ({nrow(severity_df)} rows)")

  # -- Write DuckDB analytics file --------------------------------------------
  duck_path <- file.path(out_csv_dir, paste0(CFG$run_tag, "_analytics.duckdb"))
  tryCatch({
    write_duckdb_analytics(
      duck_path        = duck_path,
      district_summary = district_summary,
      severity_df      = severity_df,
      timeseries_df    = timeseries_df,
      duckdb_threads   = CFG$duckdb_threads
    )
    log_info("DuckDB analytics  -> {basename(duck_path)} (3 tables, indexed)")
  }, error = function(e) {
    log_warn("DuckDB write failed (non-fatal -- CSVs are unaffected): {e$message}")
  })

  # -- Sanity guardrail --------------------------------------------------------
  state_area_ha <- list(
    punjab         = 5036000L,
    haryana        = 4421000L,
    uttar_pradesh  = 24093000L,
    madhya_pradesh = 30828000L,
    rajasthan      = 34224000L,
    maharashtra    = 30771000L,
    delhi          = 148000L
  )

  total_burn <- sum(district_summary$burn_area_ha, na.rm = TRUE)
  log_info("=========================================================")
  log_info("STATE TOTAL: {round(total_burn, 0)} ha ({round(total_burn/100, 0)} km^2)")
  log_info("  {nrow(filter(district_summary, burn_area_ha > 0))} of {nrow(district_summary)} districts had burn")

  ref_area <- state_area_ha[[CFG$state]]
  if (!is.null(ref_area)) {
    pct <- 100 * total_burn / ref_area
    log_info("  ~{round(pct, 1)}% of {CFG$display_name}'s total area ({format(ref_area, big.mark=',')} ha)")
    if (pct > 50) {
      log_warn("  ** SANITY WARNING ** burn area > 50% of state. Likely a methodology bug -- review before reporting.")
    } else if (pct < 0.1 && CFG$season %in% c("Rabi", "Kharif")) {
      log_warn("  ** SANITY WARNING ** burn area < 0.1% of state. Unusually low -- check thresholds and date windows.")
    }
  }
  log_info("=========================================================")

  # Console preview of top districts
  cat("\nTop 10 districts by total burn area:\n")
  preview_cols <- c("district", "burn_area_ha", "burn_pixel_count",
                    "mean_dNBR", "max_dNBR", "first_detection",
                    "mean_persistence_days")
  print(head(district_summary[, preview_cols], 10), row.names = FALSE)

  # -- Push outputs to GCS -----------------------------------------------------
  if (exists("gcs_push", mode = "function") && nchar(CFG$gcs_bucket) > 0) {
    for (path in c(dist_csv, ts_csv, sev_csv, duck_path)) {
      if (!file.exists(path)) next
      tryCatch(
        gcs_push(path, cfg = CFG, subdir = "csv"),
        error = function(e) log_warn("GCS push failed for {basename(path)}: {e$message}")
      )
    }
  }

  log_info("Step 04 complete. Outputs in {out_csv_dir}")
  invisible(list(district_summary = district_summary,
                 timeseries       = timeseries_df,
                 severity         = severity_df,
                 duckdb_path      = duck_path))
}

if (!interactive() && identical(commandArgs(TRUE)[1], "--step=04")) {
  root_dir <- normalizePath(Sys.getenv("PIPELINE_ROOT", "."))
  run_summarise(root_dir)
}
