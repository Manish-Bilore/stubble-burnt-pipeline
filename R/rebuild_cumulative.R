#!/usr/bin/env Rscript
# Rebuild cumulative_count / max_dnbr / first_burn_doy from existing per-date
# dNBR tifs, applying the CORRECTED max rule (guarded against NA propagation).
# Reads only files already on disk. No download, no GCS push. Local overwrite.
suppressPackageStartupMessages({library(terra); library(lubridate)})

args <- commandArgs(TRUE)
state    <- sub("--state=","",   grep("--state=",   args, value=TRUE))
year     <- sub("--year=","",    grep("--year=",    args, value=TRUE))
run_tag  <- sub("--run-tag=","", grep("--run-tag=", args, value=TRUE))
thr      <- as.numeric(sub("--thr=","", grep("--thr=", args, value=TRUE)))
if (length(thr)==0 || is.na(thr)) thr <- 0.10
gt <- "data/outputs/geotiff"
if (length(run_tag)==0) stop("need --run-tag (e.g. uttar_pradesh_rabi_2026_20260301_to_20260630)")

# districts = those with at least one per-date dNBR tif
date_tifs_all <- list.files(gt, pattern=paste0("^", run_tag, "_.*_[0-9]{8}_dnbr\\.tif$"), full.names=TRUE)
dist_of <- function(f) sub(paste0(".*", run_tag, "_(.*)_[0-9]{8}_dnbr\\.tif$"), "\\1", basename(f))
districts <- sort(unique(vapply(date_tifs_all, dist_of, character(1))))
cat("Rebuilding", length(districts), "districts at threshold", thr, "\n")

build_one <- function(dist) {
  fs <- sort(list.files(gt, pattern=paste0("^", run_tag, "_", dist, "_[0-9]{8}_dnbr\\.tif$"), full.names=TRUE))
  if (length(fs)==0) { cat("  ", dist, ": no date tifs, skip\n"); return(invisible(NULL)) }
  ref <- rast(fs[1])[[1]]
  first_doy <- rast(ref); values(first_doy) <- NA_real_
  cum_count <- rast(ref); values(cum_count) <- 0L
  max_dnbr  <- rast(ref); values(max_dnbr)  <- NA_real_
  for (tif in fs) {
    ds  <- regmatches(basename(tif), regexpr("[0-9]{8}(?=_dnbr)", basename(tif), perl=TRUE))
    doy <- yday(as.Date(ds, "%Y%m%d"))
    d   <- rast(tif)[[1]]
    if (!compareGeom(d, ref, stopOnError=FALSE)) d <- resample(d, ref, method="bilinear")
    ib <- (!is.na(d)) & (d >= thr)
    first_doy <- ifel(ib & is.na(first_doy), doy, first_doy)
    cum_count <- ifel(ib, cum_count + 1L, cum_count)
    # CORRECTED: guard against NA propagation from cloudy dates
    max_dnbr  <- ifel(!is.na(d) & (is.na(max_dnbr) | d > max_dnbr), d, max_dnbr)
  }
  cum_count[cum_count == 0L] <- NA
  w <- function(r,label){p<-file.path(gt,paste0(run_tag,"_",dist,"_",label,".tif"))
    writeRaster(r,p,datatype="FLT4S",gdal=c("COMPRESS=DEFLATE","TILED=YES","BLOCKXSIZE=256","BLOCKYSIZE=256"),overwrite=TRUE);p}
  w(first_doy,"first_burn_doy"); w(cum_count,"cumulative_count"); w(max_dnbr,"max_dnbr")
  nb <- global(cum_count>0,"sum",na.rm=TRUE)[[1]]
  cat("  ", dist, ":", length(fs), "dates,", nb, "burned px\n")
}
for (d in districts) build_one(d)
cat("Done. Now run Step 04 to regenerate CSV from corrected tifs.\n")
