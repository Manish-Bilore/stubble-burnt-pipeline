#!/usr/bin/env Rscript
# =============================================================================
# run_pipeline.R  —  Orchestrator v5
# Generic CLI entry point for any state / year / date range.
# =============================================================================
#
# USAGE EXAMPLES
# ──────────────
# Named season (auto dates from state registry):
#   Rscript run_pipeline.R --state=punjab --year=2024 --season=Rabi
#
# Explicit date range (custom or cross-year):
#   Rscript run_pipeline.R --state=haryana \
#       --season-start=2025-04-10 --season-end=2025-05-20 \
#       --baseline-start=2025-01-15 --baseline-end=2025-03-15
#
# Near real-time (auto-computes windows from today):
#   Rscript run_pipeline.R --state=uttar_pradesh --nrt
#   Rscript run_pipeline.R --state=punjab --nrt --nrt-lookback=45 --nrt-baseline=60
#
# Multi-state loop (bash):
#   for s in punjab haryana uttar_pradesh madhya_pradesh; do
#     Rscript run_pipeline.R --state=$s --year=2024 --season=Kharif
#   done
#
# Specific steps only:
#   Rscript run_pipeline.R --state=punjab --year=2024 --season=Rabi --steps=02:04
#
# List supported states:
#   Rscript run_pipeline.R --list-states
#
# All CLI flags:
#   --state             state code (required). See --list-states for current set.
#   --year              integer year (default: current year)
#   --season            Rabi|Kharif (default: Rabi; ignored if explicit dates given)
#   --season-start      ISO date YYYY-MM-DD
#   --season-end        ISO date YYYY-MM-DD
#   --baseline-start    ISO date YYYY-MM-DD
#   --baseline-end      ISO date YYYY-MM-DD
#   --nrt               flag — near real-time mode
#   --nrt-lookback      days back for post-fire window (default: 30)
#   --nrt-baseline      days for baseline window (default: 45)
#   --stac              CDSE|MPC (default: CDSE)
#   --cloud             max cloud % for STAC search (default: 70)
#   --threshold         dNBR burn threshold (default: 0.10)
#   --workers           parallel workers (default: 4)
#   --gcs-bucket        GCS bucket name for intermediates (default: from GCS_BUCKET env)
#   --steps             steps to run: "01:04" | "02,03" | "04" (default: 01:04)
#   --list-states       print all supported states and exit
# =============================================================================

suppressPackageStartupMessages(library(logger))

# ── Resolve project root ─────────────────────────────────────────────────────
ROOT <- normalizePath(
  Sys.getenv("PIPELINE_ROOT",
    unset = tryCatch(dirname(normalizePath(
      sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])
    )), error = function(e) getwd())
  ), mustWork = FALSE
)
if (!file.exists(file.path(ROOT, "R", "config.R"))) ROOT <- getwd()

# ── Parse CLI arguments ──────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name, default = NULL) {
  pattern <- paste0("^--", name, "=(.+)$")
  hit     <- grep(pattern, args, value = TRUE, perl = TRUE)
  if (length(hit) == 0) return(default)
  sub(pattern, "\\1", hit[length(hit)], perl = TRUE)  # last wins
}

flag_set <- function(name) any(args == paste0("--", name))

# --list-states
if (flag_set("list-states")) {
  source(file.path(ROOT, "R", "state_registry.R"))
  list_states()
  quit(status = 0)
}

# Required
state <- get_arg("state")
if (is.null(state)) stop("--state is required. Run with --list-states to see options.")

# Optional with defaults
year            <- as.integer(get_arg("year",         format(Sys.Date(), "%Y")))
season          <- get_arg("season",                  "Rabi")
season_start    <- get_arg("season-start",            NULL)
season_end      <- get_arg("season-end",              NULL)
baseline_start  <- get_arg("baseline-start",          NULL)
baseline_end    <- get_arg("baseline-end",            NULL)
nrt             <- flag_set("nrt")
nrt_lookback    <- as.integer(get_arg("nrt-lookback", "30"))
nrt_baseline    <- as.integer(get_arg("nrt-baseline", "45"))
stac_source     <- get_arg("stac",                    "CDSE")
max_cloud_pct   <- as.integer(get_arg("cloud",        "70"))
dnbr_burn_min   <- as.numeric(get_arg("threshold",    "0.10"))
n_workers       <- as.integer(get_arg("workers",      "4"))
# GCS bucket: CLI arg overrides; otherwise read from env; otherwise empty.
gcs_bucket      <- get_arg("gcs-bucket",              Sys.getenv("GCS_BUCKET", ""))
steps_arg       <- get_arg("steps",                   "01:04")

# ── Resolve steps ────────────────────────────────────────────────────────────
all_steps <- c("01", "02", "03", "04")
steps_to_run <- if (grepl(":", steps_arg)) {
  ends <- as.integer(strsplit(steps_arg, ":")[[1]])
  all_steps[ends[1]:ends[2]]
} else {
  sprintf("%02d", as.integer(trimws(strsplit(steps_arg, ",")[[1]])))
}

# ── Source config, credentials, and helpers ──────────────────────────────────
source(file.path(ROOT, "credentials", "CDSE_api.R"))
source(file.path(ROOT, "credentials", "earthdata_api.R"))
source(file.path(ROOT, "R", "config.R"))
source(file.path(ROOT, "R", "state_registry.R"))
source(file.path(ROOT, "R", "00_gcs_utils.R"))   # was orphaned in v4; now sourced

CFG <- build_config(
  state          = state,
  year           = year,
  season         = season,
  season_start   = season_start,
  season_end     = season_end,
  baseline_start = baseline_start,
  baseline_end   = baseline_end,
  nrt            = nrt,
  nrt_lookback   = nrt_lookback,
  nrt_baseline   = nrt_baseline,
  stac_source    = stac_source,
  max_cloud_pct  = max_cloud_pct,
  dnbr_burn_min  = dnbr_burn_min,
  n_workers      = n_workers,
  gcs_bucket     = gcs_bucket,
  root_dir       = ROOT
)

# ── Create required directories ──────────────────────────────────────────────
for (d in c(CFG$dir_logs, CFG$dir_tmp, CFG$dir_raw_gfsad,
            CFG$dir_dnbr, CFG$dir_baselines,
            CFG$dir_out_tif, CFG$dir_out_csv)) {
  dir.create(file.path(ROOT, d), recursive = TRUE, showWarnings = FALSE)
}

# ── Set up run log ───────────────────────────────────────────────────────────
log_file <- file.path(ROOT, CFG$dir_logs,
                      paste0(CFG$run_tag, "_pipeline.log"))
log_appender(appender_tee(log_file))
log_threshold(INFO)
log_info("========================================")
log_info("RUN: {CFG$run_id}")
log_info("Steps: {paste(steps_to_run, collapse=', ')}")
log_info("Log: {log_file}")
log_info("========================================")

# ── Step registry ────────────────────────────────────────────────────────────
step_defs <- list(
  "01" = list(
    script = "R/01_download_gfsad.R",
    fn     = "run_step_01",
    label  = "Download GFSAD30 cropland mask"
  ),
  "02" = list(
    script = "R/02_compute_dnbr.R",
    fn     = "run_compute_dnbr",
    label  = "Stream S2 COGs + compute dNBR (parallel over tiles)"
  ),
  "03" = list(
    script = "R/03_mosaic_and_clip.R",
    fn     = "run_mosaic_and_clip",
    label  = "Mosaic tiles + clip to districts (parallel over districts)"
  ),
  "04" = list(
    script = "R/04_summarise.R",
    fn     = "run_summarise",
    label  = "exactextractr zonal stats + DuckDB timeseries → CSV"
  )
)

# ── Execute ──────────────────────────────────────────────────────────────────
timing <- list()

for (step in steps_to_run) {
  def <- step_defs[[step]]
  if (is.null(def)) { log_warn("Unknown step: {step}"); next }

  cat("\n", strrep("=", 72), "\n", sep = "")
  cat(sprintf("  STEP %s — %s\n", step, def$label))
  cat(sprintf("  Run: %s\n", CFG$run_id))
  cat(strrep("=", 72), "\n", sep = "")

  source(file.path(ROOT, def$script))
  fn <- get(def$fn)
  t0 <- proc.time()

  tryCatch(
    fn(ROOT, CFG),
    error = function(e) {
      log_error("STEP {step} FAILED: {conditionMessage(e)}")
      log_error(paste(capture.output(traceback()), collapse = "\n"))
      if (!identical(Sys.getenv("PIPELINE_CONTINUE_ON_ERROR"), "true"))
        stop(e)
    }
  )

  elapsed        <- round((proc.time() - t0)["elapsed"] / 60, 1)
  timing[[step]] <- elapsed
  log_info("Step {step} completed in {elapsed} min")
}

# ── Summary ──────────────────────────────────────────────────────────────────
cat("\n", strrep("─", 72), "\n", sep = "")
cat(sprintf("PIPELINE COMPLETE — %s\n", CFG$run_id))
for (s in names(timing))
  cat(sprintf("  Step %s (%s): %.1f min\n", s, step_defs[[s]]$label, timing[[s]]))
cat(sprintf("  TOTAL: %.1f min\n", sum(unlist(timing))))
cat(sprintf("  Outputs: %s\n", file.path(ROOT, CFG$dir_out_csv)))
cat(strrep("─", 72), "\n", sep = "")
