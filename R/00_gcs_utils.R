# =============================================================================
# 00_gcs_utils.R
# Stubble Burnt Area Mapping — Google Cloud Storage helper functions
#
# USAGE:
#   Optional. Only used when CFG$gcs_bucket is set in run_pipeline.R.
#   Set GCS_BUCKET in ~/.Renviron to enable automatic uploads after each step.
#   Leave GCS_BUCKET blank to write outputs to disk only.
#
# AUTH:
#   On a GCE VM with the correct service account: no setup needed (ADC).
#   On a local machine: set GCS_AUTH_FILE to the path of your service
#   account JSON key, or run: gcloud auth application-default login
#
# Dependencies: googleCloudStorageR
# =============================================================================

suppressPackageStartupMessages({
  library(googleCloudStorageR)
})

# =============================================================================
# Initialise GCS connection
# =============================================================================

gcs_init <- function(bucket = Sys.getenv("GCS_BUCKET", "")) {
  if (nchar(bucket) == 0) {
    message("[GCS] GCS_BUCKET not set — skipping GCS auth. Outputs will be written to disk only.")
    return(invisible(NULL))
  }

  auth_file <- Sys.getenv("GCS_AUTH_FILE", "")

  if (nchar(auth_file) > 0 && file.exists(auth_file)) {
    # Explicit service account key (local development)
    gcs_auth(auth_file)
    message("[GCS] Auth: service account key (", basename(auth_file), ")")
  } else {
    # Application Default Credentials — works automatically on GCE
    tryCatch({
      googleAuthR::gar_gce_auth()
      message("[GCS] Auth: Application Default Credentials (GCE)")
    }, error = function(e) {
      stop("[GCS] Auth failed. On GCE: ensure the VM has Storage scope. ",
           "Locally: set GCS_AUTH_FILE in ~/.Renviron or run: ",
           "gcloud auth application-default login")
    })
  }

  gcs_global_bucket(bucket)
  message("[GCS] Bucket: ", bucket)
  invisible(bucket)
}

# =============================================================================
# Upload a local file to GCS
# =============================================================================

gcs_upload_file <- function(local_path,
                             gcs_key  = NULL,
                             type     = "application/octet-stream") {
  bucket <- Sys.getenv("GCS_BUCKET", "")
  if (nchar(bucket) == 0) return(invisible(NULL))
  if (!file.exists(local_path)) {
    warning("[GCS] File not found: ", local_path)
    return(invisible(NULL))
  }
  if (is.null(gcs_key)) gcs_key <- basename(local_path)

  gcs_uri <- paste0("gs://", bucket, "/", gcs_key)
  message("[GCS] Uploading: ", basename(local_path), " → ", gcs_uri)

  # Use gsutil cp for reliability with large raster files
  ret <- system2("gsutil", args = c("cp", shQuote(local_path), shQuote(gcs_uri)),
                 stdout = FALSE, stderr = FALSE)
  if (ret != 0) warning("[GCS] Upload failed: ", local_path)
  invisible(gcs_key)
}

# =============================================================================
# Upload all files in a local directory matching a pattern
# =============================================================================

gcs_upload_dir <- function(local_dir,
                            gcs_prefix = "",
                            pattern    = NULL) {
  bucket <- Sys.getenv("GCS_BUCKET", "")
  if (nchar(bucket) == 0) return(invisible(character(0)))

  files <- list.files(local_dir, full.names = TRUE, recursive = FALSE)
  if (!is.null(pattern)) files <- files[grepl(pattern, files)]
  if (length(files) == 0) {
    message("[GCS] No files to upload in: ", local_dir)
    return(invisible(character(0)))
  }

  gcs_keys <- vapply(files, function(f) {
    key <- if (nchar(gcs_prefix) > 0) paste0(gcs_prefix, "/", basename(f)) else basename(f)
    gcs_upload_file(f, key)
    key
  }, character(1))

  invisible(gcs_keys)
}

# =============================================================================
# Download a single GCS object to a local path
# Caches by default (skip re-download if file exists and force = FALSE)
# =============================================================================

gcs_download_file <- function(gcs_key,
                               local_dir = file.path(tempdir(), "gcs_cache"),
                               force     = FALSE) {
  bucket <- Sys.getenv("GCS_BUCKET", "")
  if (nchar(bucket) == 0) stop("[GCS] GCS_BUCKET not set.")

  dir.create(local_dir, showWarnings = FALSE, recursive = TRUE)
  local_path <- file.path(local_dir, basename(gcs_key))

  if (!file.exists(local_path) || force) {
    message("[GCS] Downloading: ", gcs_key, " → ", local_path)
    gcs_get_object(gcs_key, saveToDisk = local_path, overwrite = TRUE)
  } else {
    message("[GCS] Cache hit: ", local_path)
  }
  local_path
}

# =============================================================================
# Standard GCS path builder for this pipeline
# =============================================================================

stubble_gcs_path <- list(

  geotiff = function(run_tag, filename) {
    paste0("outputs/", run_tag, "/geotiff/", filename)
  },

  csv = function(run_tag, filename) {
    paste0("outputs/", run_tag, "/csv/", filename)
  },

  gfsad = function(state_code) {
    paste0("data/gfsad/", state_code, "_gfsad30_20m.tif")
  }
)

# =============================================================================
# Convenience: push a pipeline output file to GCS if bucket is configured
# (Drop-in replacement for the gcs_push() in config.R)
# =============================================================================

gcs_push <- function(local_path, cfg, subdir = "") {
  bucket <- if (!is.null(cfg$gcs_bucket)) cfg$gcs_bucket else Sys.getenv("GCS_BUCKET", "")
  if (nchar(bucket) == 0) return(invisible(NULL))
  if (!requireNamespace("googleCloudStorageR", quietly = TRUE)) return(invisible(NULL))

  key <- paste0(
    if (!is.null(cfg$gcs_prefix)) cfg$gcs_prefix else "pipeline",
    "/", cfg$run_tag,
    if (nchar(subdir) > 0) paste0("/", subdir) else "",
    "/", basename(local_path)
  )
  gcs_upload_file(local_path, gcs_key = key)
  invisible(NULL)
}
