#!/usr/bin/env Rscript
# =============================================================================
# setup_shapefiles.R  \u2014 GeoPackage validation utility
# =============================================================================
# USAGE:
#   Rscript setup_shapefiles.R                   # validate all states
#   Rscript setup_shapefiles.R --state=punjab    # validate one state
#   Rscript setup_shapefiles.R --check-only      # report without aborting
#
# EXPECTED FILES in data/raw/shapefiles/:
#   punjab_admin.gpkg          haryana_admin.gpkg
#   uttar_pradesh_admin.gpkg   madhya_pradesh_admin.gpkg
#   rajasthan_admin.gpkg       maharashtra_admin.gpkg
#   delhi_admin.gpkg
#
# EXPECTED LAYERS in each .gpkg:
#   state_boundary    \u2192 STATE column
#   district_boundary \u2192 DISTRICT column
# =============================================================================

suppressPackageStartupMessages(library(sf))

ROOT <- normalizePath(Sys.getenv("PIPELINE_ROOT", getwd()), mustWork = FALSE)
source(file.path(ROOT, "R", "state_registry.R"))

args       <- commandArgs(trailingOnly = TRUE)
state_arg  <- sub("--state=", "", grep("--state=", args, value = TRUE))
check_only <- any(args == "--check-only")
shp_dir    <- file.path(ROOT, "data/raw/shapefiles")



states_to_check <- if (length(state_arg)) {
  tolower(trimws(gsub("[ -]", "_", state_arg)))
} else {
  names(STATE_REGISTRY)
}


cat(sprintf("\
\u2500\u2500 GeoPackage Validation \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\
"))
cat(sprintf("   Root      : %s\
", ROOT))
cat(sprintf("   Shp dir   : %s\
\
", shp_dir))

results <- list()
all_ok   <- TRUE

for (st in states_to_check) {
  info <- tryCatch(get_state_info(st),
                   error = function(e) { cat("SKIP unknown state:", st, "\
"); NULL })
  if (is.null(info)) next

  cat(sprintf("\u2500\u2500 %s (%s) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\
",
              info$display_name, info$gpkg_file))
  gpkg_path <- file.path(shp_dir, info$gpkg_file)

  if (!file.exists(gpkg_path)) {
    cat(sprintf("  [MISSING]  %s\
\
", gpkg_path))
    results[[st]] <- "MISSING"
    all_ok <- FALSE
    next
  }

  file_mb <- round(file.size(gpkg_path) / 1e6, 1)
  cat(sprintf("  File      : %s (%.1f MB)\
", info$gpkg_file, file_mb))

  # Check both layers
  avail_layers <- tryCatch(st_layers(gpkg_path)$name,
                            error = function(e) character(0))
  cat(sprintf("  Layers    : %s\
", paste(avail_layers, collapse=", ")))

  state_ok <- TRUE
  for (lyr_type in c("dist", "state")) {
    lyr_name <- info[[paste0("gpkg_lyr_", lyr_type)]]
    expected_col <- if (lyr_type == "dist") info$dist_col else "STATE"

    if (!lyr_name %in% avail_layers) {
      cat(sprintf("  [MISSING LAYER] '%s'\
", lyr_name))
      state_ok <- FALSE; all_ok <- FALSE; next
    }

    sf_obj <- tryCatch(
      st_read(gpkg_path, layer = lyr_name, quiet = TRUE),
      error = function(e) { cat(sprintf("  [READ ERROR] %s: %s\
", lyr_name, e$message)); NULL }
    )
    if (is.null(sf_obj)) { state_ok <- FALSE; all_ok <- FALSE; next }

    n_feat   <- nrow(sf_obj)
    crs_str  <- tryCatch(st_crs(sf_obj)$input, error = function(e) "unknown")
    geom_str <- paste(unique(st_geometry_type(sf_obj)), collapse=",")
    has_col  <- expected_col %in% names(sf_obj)
    n_inv    <- sum(!st_is_valid(sf_obj))

    status <- if (has_col && n_inv == 0) "[OK]   " else "[WARN] "
    if (!has_col || n_inv > 0) { state_ok <- FALSE; all_ok <- FALSE }

    cat(sprintf("  %s layer:%-20s n=%-4d  CRS: %-12s  geom: %-14s  col '%s': %s\
",
                status, lyr_name, n_feat, crs_str, geom_str,
                expected_col, if (has_col) "found" else "MISSING"))

    if (!has_col)
      cat(sprintf("         \u21b3 Available columns: %s\
",
                  paste(setdiff(names(sf_obj), "geom"), collapse=", ")))
    if (n_inv > 0)
      cat(sprintf("         \u21b3 %d invalid geometries \u2014 auto-fixed by st_make_valid() at runtime\
", n_inv))

    # Print sample district names
    if (lyr_type == "dist" && has_col) {
      dnames <- head(sort(unique(sf_obj[[expected_col]])), 5)
      cat(sprintf("         \u21b3 Sample districts: %s\
", paste(dnames, collapse=", ")))
    }
  }
  results[[st]] <- if (state_ok) "OK" else "WARN"
  cat("\
")
}

# Summary
n_ok      <- sum(unlist(results) == "OK")
n_missing <- sum(unlist(results) == "MISSING")
n_warn    <- sum(unlist(results) == "WARN")

cat(strrep("\u2500", 60), "\
")
cat(sprintf("RESULT: %d/%d OK | %d MISSING | %d WARN\
",
            n_ok, length(states_to_check), n_missing, n_warn))

if (n_missing > 0) {
  cat("\
Missing GeoPackages \u2014 upload these to data/raw/shapefiles/:\
")
  for (st in names(results)[results == "MISSING"])
    cat(sprintf("  \u2022 %s\
", STATE_REGISTRY[[st]]$gpkg_file))
}
if (n_warn > 0) {
  cat("\
Warnings \u2014 check layer/column names above\
")
}


if (!check_only && !all_ok) {
  message("\nFix missing files or run with --check-only to suppress exit.")
} else {
  cat("\nAll checks passed. Pipeline is ready to run.\n")
}
