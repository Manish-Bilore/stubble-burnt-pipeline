# UP Rabi 2026 dNBR — run v1 (2026-09-08) vs run v2 (2026-09-17)

**Analysis id (identical in both):** `uttar_pradesh_rabi_2026_20260301_to_20260630__mask_worldcover__near`
**Compared artefacts:** `per_raster_summary.csv`, `district_summary.csv`, `state_rollup.csv`, `manifest.json`
**Companion CSV:** `v1_v2_district_comparison.csv` (75 districts × 11 thresholds, ranked)
**Companion script:** `diag_v1v2_tile_drop.R` (tile-level hole attribution from the two GeoTIFF sets)

---

## 1. What is held constant

| item | v1 | v2 |
|---|---|---|
| run started | 2026-09-08 14:40 | 2026-09-17 10:56 |
| post-process duration | 7.0 min (4 workers) | 24.5 min (1 worker) |
| date source | `explicit` | `preset` |
| mask sha256 | `599c8aa5…` | `599c8aa5…` (identical) |
| thresholds | 11 values, identical | identical |
| admin gpkg / n districts | 75 | 75 |
| R / GDAL | 4.6.1 / 3.8.4 | 4.6.1 / 3.8.4 |
| input rasters | 75 `*_max_dnbr.tif` | 75 `*_max_dnbr.tif` |
| results block | 75 ok, 0 failed | 75 ok, 0 failed |

Mask, thresholds, boundaries and post-processing code are the same. **Every difference below originates upstream, in the `max_dnbr` rasters themselves** (steps 02/03), not in `20_process.R` / `30_outputs.R`.

One caveat on provenance: v1 used `dates_from=explicit`, v2 `dates_from=preset`. Both resolve to 2026-03-01→2026-06-30 in the run id, but the baseline window is not recorded in the manifest, so an unnoticed baseline-window difference between the two runs cannot be excluded from these files alone.

---

## 2. State level

Observed cropland (`cropland_ha`, i.e. mask ∩ district ∩ `max_dnbr` not NA), district-clipped:

| | v1 | v2 | Δ |
|---|---|---|---|
| observed cropland | 13.890 Mha | 15.634 Mha | **+1.744 Mha (+12.6%)** |
| coverage of on-grid mask (17.234 Mha) | 80.6% | 90.7% | +10.1 pp |
| unobserved (hole) | 3.34 Mha | 1.60 Mha | −1.74 Mha |

Burnt area and burnt fraction, all thresholds (per-raster roll-up, as in `state_rollup.csv`):

| threshold | burnt v1 (Mha) | burnt v2 (Mha) | Δ (Mha) | rate v1 (%) | rate v2 (%) | share of Δ from rate |
|---|---|---|---|---|---|---|
| 0.16 | 11.216 | 13.502 | +2.287 | 80.64 | 86.24 | 38% |
| 0.27 | 9.726 | 11.982 | +2.256 | 69.93 | 76.53 | 46% |
| 0.3804 | 7.701 | 9.955 | +2.254 | 55.37 | 63.58 | 57% |
| 0.52 | 3.955 | 5.877 | +1.922 | 28.44 | 37.54 | 74% |
| 0.54 | 3.300 | 5.102 | +1.803 | 23.72 | 32.59 | 77% |
| 0.56 | 2.631 | 4.287 | +1.656 | 18.92 | 27.38 | 80% |
| 0.59 | 1.662 | 3.032 | +1.370 | 11.95 | 19.37 | 85% |
| 0.62 | 0.860 | 1.866 | +1.006 | 6.18 | 11.92 | 89% |
| 0.64 | 0.484 | 1.237 | +0.752 | 3.48 | 7.90 | 92% |
| 0.66 | 0.245 | 0.779 | +0.533 | 1.76 | 4.97 | 94% |
| 0.70 | 0.052 | 0.307 | +0.255 | 0.38 | 1.96 | 97% |

Decomposition: `Δburnt = (ΔobservedCropland × rate_v1) + (observedCropland_v2 × Δrate)`. The last column is the second term's share.

**The headline: the extra burnt area is mostly not the coverage fix.** At every threshold ≥0.3804 more than half the increase comes from the dNBR distribution itself shifting right, and at the high end almost all of it does.

---

## 3. Two independent effects, in opposite directions

### 3.1 Coverage — net +1.74 Mha, but bidirectional

| class (±5% observed-cropland change) | n districts | Σ Δ |
|---|---|---|
| coverage gain | 22 | **+2.437 Mha** |
| coverage loss | 11 | **−0.693 Mha** |
| stable | 42 | ≈ 0 |

**Largest gains** (observed cropland, ha):

| district | v1 | v2 | Δ | ×  |
|---|---|---|---|---|
| KHERI | 205,908 | 458,362 | +252,454 | 2.2 |
| MAHOBA | 31,276 | 256,174 | +224,899 | 8.2 |
| KANPUR_DEHAT | 43,817 | 243,550 | +199,734 | 5.6 |
| HAMIRPUR | 118,211 | 307,688 | +189,477 | 2.6 |
| KANNAUJ | 446 | 163,432 | +162,986 | 366 |
| SAHARANPUR | 7 | 155,889 | +155,883 | 22,925 |
| MATHURA | 147,685 | 277,810 | +130,124 | 1.9 |
| HARDOI | 319,982 | 442,497 | +122,515 | 1.4 |
| ALIGARH | 192,693 | 313,898 | +121,205 | 1.6 |
| MEERUT | 40,442 | 158,217 | +117,776 | 3.9 |
| BULANDSHAHR | 175,664 | 290,233 | +114,569 | 1.7 |
| MUZAFFARNAGAR | 29,700 | 139,115 | +109,415 | 4.7 |
| BAGHPAT | 761 | 94,844 | +94,083 | 125 |
| SHAMLI | 1,044 | 84,667 | +83,623 | 81 |
| SITAPUR | 320,085 | 390,441 | +70,356 | 1.2 |
| GAUTAM_BUDDHA_NAGAR | 2,310 | 69,695 | +67,384 | 30 |
| GHAZIABAD | 331 | 51,953 | +51,622 | 157 |
| HAPUR | 31,094 | 80,515 | +49,422 | 2.6 |
| FATEHPUR | 257,241 | 305,827 | +48,586 | 1.2 |
| SONBHADRA | 200,671 | 231,778 | +31,106 | 1.2 |

Six districts were effectively **absent** from v1 — SAHARANPUR 7 ha, GHAZIABAD 331 ha, KANNAUJ 446 ha, BAGHPAT 761 ha, SHAMLI 1,044 ha, GAUTAM_BUDDHA_NAGAR 2,310 ha — i.e. v1's western-UP numbers were meaningless, not merely low. The far-western cluster (Saharanpur, Shamli, Baghpat, Muzaffarnagar, Meerut, Ghaziabad, GB Nagar) sits in UTM zone 43N, which is the exact signature of the mask-CRS bug (mask built in 32644, applied to off-zone tiles with `resample()` instead of `project()`). The central-UP gains (Kannauj, Mahoba, Kanpur Dehat, Hamirpur, Kheri, Hardoi — all zone 44N) cannot be that bug and must come from baseline/date coverage instead. The tile script separates the two.

**All losses** (observed cropland, ha):

| district | v1 | v2 | Δ | ratio |
|---|---|---|---|---|
| UNNAO | 318,783 | 124,807 | −193,976 | 0.39 |
| PRAYAGRAJ | 372,026 | 292,769 | −79,257 | 0.79 |
| AZAMGARH | 308,677 | 231,906 | −76,771 | 0.75 |
| JAUNPUR | 298,894 | 236,507 | −62,387 | 0.79 |
| VARANASI | 96,150 | 41,622 | −54,528 | 0.43 |
| LUCKNOW | 101,625 | 55,716 | −45,909 | 0.55 |
| CHITRAKOOT | 189,178 | 155,637 | −33,541 | 0.82 |
| RAE_BARELI | 269,507 | 245,203 | −24,304 | 0.91 |
| PRATAPGARH | 244,430 | 232,589 | −11,841 | 0.95 |
| KANPUR_NAGAR | 196,861 | 186,839 | −10,022 | 0.95 |
| GONDA | 272,391 | 263,291 | −9,099 | 0.97 |
| SULTANPUR | 180,897 | 172,309 | −8,588 | 0.95 |

**This is the most consequential finding of the comparison.** That list is, almost exactly, the outlier list in the gap diagnosis: UNNAO 0.38, VARANASI 0.42, LUCKNOW 0.46, AZAMGARH 0.70, PRAYAGRAJ 0.76, CHITRAKOOT 0.78, JAUNPUR 0.78. Those districts were *not* holed in v1. Mapped through the diagnosis's district→tile table, the holes that appear in v2 belong to **44RMQ** (UNNAO, LUCKNOW, KANPUR_NAGAR, RAE_BARELI), **44RNN** (PRAYAGRAJ, CHITRAKOOT) and **44RPP/44RPQ** (AZAMGARH, JAUNPUR, VARANASI).

So the §8 open item "44RMQ unexplained — the picks should cover the tile" now has a sharper form: **44RMQ was covered in v1 and lost coverage in v2**, with the same STAC catalogue and the same Feb-2026 candidate pool in both. A static candidate-ranking defect cannot by itself produce a run-to-run change; a cache state change can (baseline rebuilt in v2 from a different pick order or a partially failed band stream, or v1 having reused a good cached baseline that was invalidated before v2).

By contrast, MAHRAJGANJ (58,097 → 56,886) and PILIBHIT (153,988 → 151,564) are essentially unchanged and bad in both — **44RQR** and **44RMS** are persistent failures, not regressions.

Combining the two runs pixel-wise would give 16.35 Mha observed (94.9% of the on-grid mask) against 90.7% for v2 alone — useful as a diagnostic upper bound on what the imagery actually supports, but not a product: the two runs may not share a baseline, so their dNBR values are not necessarily on the same scale.

### 3.2 Rate — the dNBR distribution moved right, everywhere

Restricting to the **19 districts whose observed cropland changed by <1%** (coverage held constant, so this is a pure per-pixel comparison):

| threshold | burnt % of observed cropland, v1 | v2 | factor |
|---|---|---|---|
| 0.16 | 83.52 | 90.41 | ×1.08 |
| 0.27 | 72.53 | 81.82 | ×1.13 |
| 0.3804 | 56.92 | 68.49 | ×1.20 |
| 0.52 | 27.85 | 39.47 | ×1.42 |
| 0.54 | 22.94 | 34.00 | ×1.48 |
| 0.59 | 11.15 | 19.78 | ×1.77 |
| 0.62 | 5.63 | 11.97 | ×2.13 |
| 0.66 | 1.52 | 4.70 | ×3.09 |
| 0.70 | 0.32 | 1.64 | **×5.07** |

The shift is monotonic in threshold — the upper tail inflates far more than the median. Two mechanisms produce exactly this shape, and the CSVs cannot separate them:

1. **More post-fire dates per pixel.** `max_dnbr` is a running maximum, so adding valid observations can only raise it, and the effect is concentrated in the tail (more draws → higher maximum). Consistent with the dedup indexing fix and the `prev_date` ratchet fix, both of which restore dropped dates.
2. **A lower/greener baseline NBR.** A different baseline composite shifts the whole dNBR distribution by a constant per pixel.

Either way, **v2's higher burnt totals at high thresholds are not validated improvements.** A 5× increase in pixels above dNBR 0.70 between two runs of an uncalibrated threshold is a measurement-stability problem: it says the number of observations per pixel is a first-order control on the reported burnt area, which is exactly the property a max-composite should not have if the thresholds meant anything physically. It also compounds the known harvest-commission problem — more observations means more chances to catch a harvest-driven NBR drop, not just a char signal.

---

## 4. Secondary, unchanged between runs

- `district_summary.csv` sliver rows grew 405 → 441 (337 → 373 neighbour-sliver rows): more rows because the v2 rasters cover more area and therefore overlap more neighbours. Not a defect in itself, but per-raster state roll-up still double-counts boundary cells (+19,187 ha in v1, +22,352 ha in v2 relative to the district-clipped sum).
- `raster_district` uses underscores, `district` uses spaces, so the 7 multi-word districts (AMBEDKAR NAGAR, BARA BANKI, GAUTAM BUDDHA NAGAR, KANPUR DEHAT, KANPUR NAGAR, RAE BARELI, SANT KABIR NAGAR) never self-match. Present identically in both runs; it will silently break any own-district-vs-sliver filter written against these files.
- `cropland_ha` still means *observed* cropland in both runs, so every `pct_cropland_*` uses a per-district, per-run denominator. Between v1 and v2 that denominator moved by up to ×366 (KANNAUJ) and ×0.39 (UNNAO), so **percentage columns are not comparable across runs, and burnt-hectare deltas in the 33 non-stable districts conflate coverage with detection.** Only the 42 stable districts support a like-for-like reading.

---

## 5. What the two GeoTIFF sets should be asked next

`diag_v1v2_tile_drop.R` computes, per district and per MGRS tile, from both raster sets: cropland, hole in v1, hole in v2, persistent, fixed-in-v2, new-in-v2, plus a 100 m 4-band GeoTIFF for QGIS. Set `DIR_V1`/`DIR_V2` at the top; it reuses `mgrs_tiles` from `diag_step3_holes.gpkg`. Expected discriminations:

1. **Confirm the regression set** — does the new-hole mass concentrate on 44RMQ / 44RNN / 44RPP / 44RPQ, and does its geometry still stop at swath edges and tile-exclusive areas?
2. **Separate the two gain mechanisms** — western zone-43 districts should show a v1 hole shaped like the *whole tile* (mask wipe), central zone-44 districts a v1 hole shaped like a *swath edge* (baseline coverage).
3. **Rank tiles by `new_ha`** to produce the exact cache-invalidation list for the baseline fix, instead of rebuilding all 76 tiles.

Then, on the VM, for every tile with `new_ha > 0`, compare the two baselines directly — `gdalinfo -stats` VALID_PERCENT, file mtime, and the logged pick list — since a tile that was covered in v1 and holed in v2 is the cleanest available test of whether the failure is candidate ranking (§7 mechanisms 1–3), silent stream loss (mechanism 4), or stale/invalidated cache (mechanism 5).

---

## 6. Bottom line

- v2 is a **net coverage improvement**: 80.6% → 90.7% of on-grid cropland observed, and six western districts go from unusable to usable.
- v2 also **introduced ~0.69 Mha of new holes** in central and eastern UP, concentrated in the districts the gap diagnosis then flagged. Those holes are a regression from v1, not a pre-existing condition; 44RQR and 44RMS are the only genuinely persistent failures.
- The burnt-area increase (+2.29 Mha at 0.16, +1.00 Mha at 0.62) is **majority-driven by a rightward shift in dNBR**, not by the coverage gain, and is therefore not evidence of a better burn estimate.
- Neither run is reportable: thresholds remain uncalibrated for Indian cropland, no char confirmation, and the denominator issue is unfixed in both.
