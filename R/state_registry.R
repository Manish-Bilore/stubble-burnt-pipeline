# =============================================================================
# state_registry.R
# Central registry for all supported Indian states.
# GeoPackage format: <state_code>_admin.gpkg
#   Layer "state_boundary"   \u2192 STATE column
#   Layer "district_boundary" \u2192 DISTRICT column
# =============================================================================

STATE_REGISTRY <- list(

  punjab = list(
    display_name   = "Punjab",
    state_code     = "punjab",
    utm_crs        = "EPSG:32643",
    gpkg_file      = "punjab_admin.gpkg",
    gpkg_lyr_dist  = "district_boundary",
    gpkg_lyr_state = "state_boundary",
    dist_col       = "DISTRICT",
    rabi   = list(baseline_start="02-01", baseline_end="02-28",
                  season_start="03-01",   season_end="06-30"),
    kharif = list(baseline_start="08-01", baseline_end="09-15",
                  season_start="10-01",   season_end="12-31")
  ),

  haryana = list(
    display_name   = "Haryana",
    state_code     = "haryana",
    utm_crs        = "EPSG:32643",
    gpkg_file      = "haryana_admin.gpkg",
    gpkg_lyr_dist  = "district_boundary",
    gpkg_lyr_state = "state_boundary",
    dist_col       = "DISTRICT",
    rabi   = list(baseline_start="02-01", baseline_end="02-28",
                  season_start="03-01",   season_end="06-30"),
    kharif = list(baseline_start="08-01", baseline_end="09-15",
                  season_start="10-01",   season_end="12-31")
  ),

  uttar_pradesh = list(
    display_name   = "Uttar Pradesh",
    state_code     = "uttar_pradesh",
    utm_crs        = "EPSG:32644",
    gpkg_file      = "uttar_pradesh_admin.gpkg",
    gpkg_lyr_dist  = "district_boundary",
    gpkg_lyr_state = "state_boundary",
    dist_col       = "DISTRICT",
    rabi   = list(baseline_start="02-01", baseline_end="02-28",
                  season_start="03-01",   season_end="06-30"),
    kharif = list(baseline_start="08-01", baseline_end="09-20",
                  season_start="10-01",   season_end="12-31")
  ),

  madhya_pradesh = list(
    display_name   = "Madhya Pradesh",
    state_code     = "madhya_pradesh",
    utm_crs        = "EPSG:32643",
    gpkg_file      = "madhya_pradesh_admin.gpkg",
    gpkg_lyr_dist  = "district_boundary",
    gpkg_lyr_state = "state_boundary",
    dist_col       = "DISTRICT",
    rabi   = list(baseline_start="02-01", baseline_end="02-28",
                  season_start="03-20",   season_end="06-30"),
    kharif = list(baseline_start="08-01", baseline_end="09-20",
                  season_start="10-01",   season_end="12-31")
  ),

  rajasthan = list(
    display_name   = "Rajasthan",
    state_code     = "rajasthan",
    utm_crs        = "EPSG:32643",
    gpkg_file      = "rajasthan_admin.gpkg",
    gpkg_lyr_dist  = "district_boundary",
    gpkg_lyr_state = "state_boundary",
    dist_col       = "DISTRICT",
    rabi   = list(baseline_start="01-15", baseline_end="02-28",
                  season_start="03-15",   season_end="05-15"),
    kharif = list(baseline_start="07-15", baseline_end="09-01",
                  season_start="09-15",   season_end="11-15")
  ),

  maharashtra = list(
    display_name   = "Maharashtra",
    state_code     = "maharashtra",
    utm_crs        = "EPSG:32643",
    gpkg_file      = "maharashtra_admin.gpkg",
    gpkg_lyr_dist  = "district_boundary",
    gpkg_lyr_state = "state_boundary",
    dist_col       = "DISTRICT",
    rabi   = list(baseline_start="01-15", baseline_end="02-28",
                  season_start="03-15",   season_end="05-15"),
    kharif = list(baseline_start="08-01", baseline_end="09-20",
                  season_start="10-01",   season_end="12-15")
  ),

  delhi = list(
    display_name   = "Delhi",
    state_code     = "delhi",
    utm_crs        = "EPSG:32643",
    gpkg_file      = "delhi_admin.gpkg",
    gpkg_lyr_dist  = "district_boundary",
    gpkg_lyr_state = "state_boundary",
    dist_col       = "DISTRICT",
    rabi   = list(baseline_start="02-01", baseline_end="02-28",
                  season_start="03-01",   season_end="05-20"),
    kharif = list(baseline_start="08-01", baseline_end="09-15",
                  season_start="10-01",   season_end="11-20")
  )
)

# \u2500\u2500 Helpers \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

get_state_info <- function(state_code) {
  key <- tolower(trimws(gsub("[ -]", "_", state_code)))
  if (!key %in% names(STATE_REGISTRY))
    stop("Unknown state: '", state_code, "'.\
Supported: ",
         paste(names(STATE_REGISTRY), collapse=", "))
  STATE_REGISTRY[[key]]
}

list_states <- function() {
  cat("Supported states:\
")
  for (k in names(STATE_REGISTRY)) {
    s <- STATE_REGISTRY[[k]]
    cat(sprintf("  %-18s  UTM: %-14s  gpkg: %s\
",
                k, s$utm_crs, s$gpkg_file))
  }
  invisible(names(STATE_REGISTRY))
}

default_season_dates <- function(state_info, season, year) {
  s <- state_info[[tolower(season)]]
  if (is.null(s)) stop("Unknown season '", season, "'. Use 'Rabi' or 'Kharif'.")
  list(
    baseline_start = paste0(year, "-", s$baseline_start),
    baseline_end   = paste0(year, "-", s$baseline_end),
    season_start   = paste0(year, "-", s$season_start),
    season_end     = paste0(year, "-", s$season_end)
  )
}

nrt_date_windows <- function(post_lookback_days = 30,
                              baseline_days      = 45,
                              reference_date     = Sys.Date()) {
  season_end     <- reference_date
  season_start   <- reference_date - post_lookback_days
  baseline_end   <- season_start   - 5
  baseline_start <- baseline_end   - baseline_days
  list(
    baseline_start = format(baseline_start, "%Y-%m-%d"),
    baseline_end   = format(baseline_end,   "%Y-%m-%d"),
    season_start   = format(season_start,   "%Y-%m-%d"),
    season_end     = format(season_end,     "%Y-%m-%d")
  )
}

#' Load district layer from state GeoPackage.
#' Returns an sf object. CRS is transformed to target_crs if provided.
load_districts <- function(root_dir, cfg, target_crs = NULL) {
  gpkg_path <- file.path(root_dir, "data/raw/shapefiles", cfg$gpkg_file)
  if (!file.exists(gpkg_path))
    stop("GeoPackage not found: ", gpkg_path,
         "\
Upload ", cfg$gpkg_file, " to data/raw/shapefiles/ and re-run.")
  sf_obj <- sf::st_read(gpkg_path, layer = cfg$gpkg_lyr_dist, quiet = TRUE)
  sf_obj <- sf::st_make_valid(sf_obj)
  if (!is.null(target_crs)) sf_obj <- sf::st_transform(sf_obj, target_crs)
  sf_obj
}

#' Load state boundary layer from state GeoPackage.
load_state_boundary <- function(root_dir, cfg, target_crs = NULL) {
  gpkg_path <- file.path(root_dir, "data/raw/shapefiles", cfg$gpkg_file)
  if (!file.exists(gpkg_path))
    stop("GeoPackage not found: ", gpkg_path)
  sf_obj <- sf::st_read(gpkg_path, layer = cfg$gpkg_lyr_state, quiet = TRUE)
  sf_obj <- sf::st_make_valid(sf_obj)
  if (!is.null(target_crs)) sf_obj <- sf::st_transform(sf_obj, target_crs)
  sf_obj
}
