#!/bin/bash
set -e
export PIPELINE_ROOT=/data/stubble_burnt
cd /data/stubble_burnt
B=gs://stubble-burnt-pipeline-bb5ecdd8/pipeline

run_one() {
  STATE=$1; SEASON=$2; YEAR=$3
  echo "=== $STATE $SEASON $YEAR START $(date -u) ==="
  rm -rf data/outputs/geotiff/* data/interim/dnbr/* data/interim/baselines/* /mnt/scratch/*
  Rscript run_pipeline.R --state=$STATE --stac=MPC --season=$SEASON --year=$YEAR --steps=01:04 --workers=6
  TAG=$(ls -1 data/outputs/geotiff | head -1 | sed 's/_[A-Z].*//')
  echo "--- archiving $TAG ---"
  gcloud storage rsync -r --delete-unmatched-destination-objects data/outputs/geotiff $B/$TAG/geotiff
  gcloud storage cp data/outputs/csv/${TAG}_* $B/$TAG/csv/
  echo "=== $STATE $SEASON $YEAR DONE $(date -u) | local:$(ls -1 data/outputs/geotiff|wc -l) bucket:$(gcloud storage ls $B/$TAG/geotiff/|wc -l) ==="
  df -h /data/stubble_burnt
}

run_one haryana Rabi 2026
run_one haryana Kharif 2025
