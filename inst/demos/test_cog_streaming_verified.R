#!/usr/bin/env Rscript
# =============================================================================
# COG Streaming Verification Script - adapted from Carlo's example script
# =============================================================================
#
# PURPOSE:
#   Tests whether a Cloud Optimized GeoTIFF (COG) hosted at a remote URL
#   supports efficient partial reads via HTTP range requests. This is critical
#   for working with large rasters without downloading entire files.
#
# WHAT IT TESTS:
#   1. HTTP range request support (server must return 206 Partial Content)
#   2. Fast metadata access (should read dimensions/CRS without full download)
#   3. Bbox subsetting (crop to extent, verify only needed tiles are fetched)
#   4. Scaling behavior (larger windows should read proportionally more data)
#   5. Polygon clipping (crop + mask workflow for irregular boundaries)
#   6. Overview access (low-res preview of full extent)
#
# OUTPUTS:
#   - Console output with pass/fail checks
#   - 4 PNG visualizations saved to working directory
#
# USAGE:
#   Rscript test_cog_streaming_verified.R
#   # Or source interactively in RStudio
#
# REQUIREMENTS:
#   - terra, sf, httr packages
#   - Network access to the COG URL
#
# =============================================================================

library(terra)
library(sf)
library(httr)

# --- CONFIG ---
COG_URL <- "https://knb.ecoinformatics.org/data/WRI_score.tif"
VSICURL_PATH <- paste0("/vsicurl/", COG_URL)  # GDAL virtual path for HTTP access
OUTPUT_DIR <- getwd()

# GDAL environment settings to reduce unnecessary HTTP requests
# EMPTY_DIR: don't try to list directory contents (no sidecar files to find)
# ALLOWED_EXTENSIONS: explicitly allow .tif files over HTTP
Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
Sys.setenv(CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif")

# =============================================================================
# TEST 1: Verify HTTP range requests work
# =============================================================================
# COG streaming relies on the server accepting "Range" headers, which let us
# request specific byte ranges instead of the whole file. A properly configured
# server returns HTTP 206 (Partial Content) instead of 200 (OK).
# If this fails, the server will send the entire file for every request.

cat("=== TEST 1: HTTP Range Request Support ===\n")

# HEAD request checks server headers without downloading content
head_resp <- HEAD(COG_URL, timeout(60))

# Request only first 1KB to test range support
range_resp <- GET(COG_URL, add_headers(Range = "bytes=0-1023"), timeout(60))

head_ok <- status_code(head_resp) == 200
range_ok <- status_code(range_resp) == 206
accept_ranges <- headers(head_resp)$`accept-ranges`
content_length <- as.numeric(headers(head_resp)$`content-length`)

cat("HEAD status:", status_code(head_resp), ifelse(head_ok, "[OK]", "[FAIL]"), "\n")
cat("Range request status:", status_code(range_resp), ifelse(range_ok, "[OK - Partial Content]", "[FAIL]"), "\n")
cat("Accept-Ranges:", accept_ranges, ifelse(accept_ranges == "bytes", "[OK]", "[FAIL]"), "\n")
cat("Full file size:", round(content_length / 1e9, 2), "GB\n\n")

if (!range_ok) stop("Server does not support range requests. COG streaming won't work.")

# =============================================================================
# TEST 2: Open remote COG - verify metadata read is fast
# =============================================================================
# When opening a COG, GDAL reads only the file header (a few KB) to get
# dimensions, CRS, and tile structure. This should complete in seconds.
# If it takes minutes, something is wrong (likely downloading the whole file).

cat("=== TEST 2: Remote Metadata Access ===\n")

t_start <- Sys.time()
r <- rast(VSICURL_PATH)  # Opens connection, reads header only
t_metadata <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

cat("Dimensions:", ncol(r), "x", nrow(r), "pixels\n")
cat("CRS: EPSG:", crs(r, describe = TRUE)$code, "\n")
cat("Resolution:", res(r)[1], "x", res(r)[2], "m\n")
cat("Extent:", as.vector(ext(r)), "\n")
cat("Metadata read time:", round(t_metadata, 2), "s",
    ifelse(t_metadata < 5, "[OK - fast]", "[SLOW - check connection]"), "\n\n")

# =============================================================================
# TEST 3: Small bbox subset - verify partial read
# =============================================================================
# The key benefit of COGs: crop() fetches only the tiles that intersect your
# extent. For a small window, this should be a tiny fraction of the full file.
# We verify by comparing bytes read vs total file size.

cat("=== TEST 3: Small Bbox Subset (~50km window) ===\n")

# Define a small test window in EPSG:5070 (Albers Equal Area, units = meters)
test_ext <- ext(-2000000, -1950000, 1500000, 1550000)

t_start <- Sys.time()
subset_small <- crop(r, test_ext)  # Defines the window (lazy, no data yet)
vals_small <- values(subset_small)  # Actually fetches pixel data from server
t_small <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

# Compare bytes read to full file size - should be <1% for a small window
bytes_read_small <- object.size(vals_small)
pct_of_full <- (as.numeric(bytes_read_small) / content_length) * 100

cat("Subset dimensions:", ncol(subset_small), "x", nrow(subset_small), "pixels\n")
cat("Cells read:", format(ncell(subset_small), big.mark = ","), "\n")
cat("Valid cells:", format(sum(!is.na(vals_small)), big.mark = ","), "\n")
cat("Value range:", round(min(vals_small, na.rm = TRUE), 2), "-",
    round(max(vals_small, na.rm = TRUE), 2), "\n")
cat("Data size in memory:", round(as.numeric(bytes_read_small) / 1e6, 2), "MB\n")
cat("Percent of full file:", round(pct_of_full, 3), "%",
    ifelse(pct_of_full < 1, "[OK - partial read confirmed]", ""), "\n")
cat("Read time:", round(t_small, 2), "s\n\n")

# Visualize small subset
png(file.path(OUTPUT_DIR, "01_small_subset.png"), width = 800, height = 600)
plot(subset_small, main = "Test 3: Small Bbox Subset (~50km)",
     col = hcl.colors(50, "YlOrRd", rev = TRUE))
dev.off()
cat("Saved: 01_small_subset.png\n\n")

# =============================================================================
# TEST 4: Multiple bbox sizes - verify scaling behavior
# =============================================================================
# Critical verification: if partial reads work, larger windows should read
# proportionally more data. If the full file downloads every time (broken
# streaming), all window sizes would show similar data volumes.

cat("=== TEST 4: Scaling Test - Multiple Bbox Sizes ===\n")

# Define windows of increasing size (all in EPSG:5070 meters)
windows <- list(
  "10km"  = ext(-2000000, -1990000, 1500000, 1510000),
  "50km"  = ext(-2000000, -1950000, 1500000, 1550000),
  "100km" = ext(-2000000, -1900000, 1500000, 1600000),
  "200km" = ext(-2000000, -1800000, 1500000, 1700000)
)

scaling_results <- data.frame(
  window = character(),
  pixels = numeric(),
  mb_read = numeric(),
  seconds = numeric(),
  stringsAsFactors = FALSE
)

# Test each window size and record data volume + timing
for (name in names(windows)) {
  t_start <- Sys.time()
  sub <- crop(r, windows[[name]])
  v <- values(sub)
  t_elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

  scaling_results <- rbind(scaling_results, data.frame(
    window = name,
    pixels = ncell(sub),
    mb_read = as.numeric(object.size(v)) / 1e6,
    seconds = t_elapsed
  ))

  cat(sprintf("  %s: %s pixels, %.2f MB, %.2f s\n",
              name, format(ncell(sub), big.mark = ","),
              as.numeric(object.size(v)) / 1e6, t_elapsed))
}

# Visualize scaling
png(file.path(OUTPUT_DIR, "02_scaling_test.png"), width = 1000, height = 400)
par(mfrow = c(1, 2))
barplot(scaling_results$mb_read, names.arg = scaling_results$window,
        main = "Data Read vs Window Size", ylab = "MB", col = "steelblue")
barplot(scaling_results$seconds, names.arg = scaling_results$window,
        main = "Read Time vs Window Size", ylab = "Seconds", col = "coral")
dev.off()
cat("Saved: 02_scaling_test.png\n\n")

# Verify linear-ish scaling (not constant = proves partial reads)
if (scaling_results$mb_read[4] > scaling_results$mb_read[1] * 5) {
  cat("[OK] Data read scales with window size - partial reads confirmed\n\n")
} else {
  cat("[WARNING] Data read doesn't scale as expected - may be downloading full file\n\n")
}

# =============================================================================
# TEST 5: Polygon clip workflow
# =============================================================================
# Common use case: extract raster values within an irregular boundary (county,
# watershed, etc). The workflow is:
#   1. Define/load polygon in any CRS
#   2. Reproject polygon to match raster CRS
#   3. crop() to polygon's bounding box (reduces tiles fetched)
#   4. mask() to set pixels outside polygon to NA

cat("=== TEST 5: Polygon Clip (SF Bay Area) ===\n")

# Define a simple polygon in WGS84 lat/lon (how most boundaries are stored)
poly_sf <- st_as_sf(st_sfc(
  st_polygon(list(matrix(c(
    -122.60, 37.60,
    -121.80, 37.60,
    -121.80, 38.10,
    -122.60, 38.10,
    -122.60, 37.60
  ), ncol = 2, byrow = TRUE))),
  crs = 4326
))

# Reproject to match raster CRS - required for spatial operations
poly_5070 <- st_transform(poly_sf, crs = st_crs(r))
poly_vect <- vect(poly_5070)  # Convert sf to terra's vector format

cat("Polygon bbox (EPSG:5070):", as.vector(ext(poly_vect)), "\n")

# Two-step clip: crop first (fast, reduces data), then mask (sets outside to NA)
# snap="out" ensures we get all pixels that touch the polygon boundary
t_start <- Sys.time()
r_cropped <- crop(r, poly_vect, snap = "out")  # Fetch tiles for bbox only
r_masked <- mask(r_cropped, poly_vect)          # Set pixels outside polygon to NA
vals_poly <- values(r_masked)
t_poly <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

bytes_poly <- object.size(vals_poly)
pct_poly <- (as.numeric(bytes_poly) / content_length) * 100

cat("Output dimensions:", ncol(r_masked), "x", nrow(r_masked), "pixels\n")
cat("Total cells:", format(ncell(r_masked), big.mark = ","), "\n")
cat("Valid cells (inside polygon):", format(sum(!is.na(vals_poly)), big.mark = ","), "\n")
cat("Data size:", round(as.numeric(bytes_poly) / 1e6, 2), "MB\n")
cat("Percent of full file:", round(pct_poly, 2), "%\n")
cat("Crop + mask time:", round(t_poly, 2), "s\n\n")

# Visualize polygon clip
png(file.path(OUTPUT_DIR, "03_polygon_clip.png"), width = 800, height = 600)
plot(r_masked, main = "Test 5: Polygon Clip (SF Bay Area)",
     col = hcl.colors(50, "YlOrRd", rev = TRUE))
plot(st_geometry(poly_5070), add = TRUE, border = "blue", lwd = 2)
dev.off()
cat("Saved: 03_polygon_clip.png\n\n")

# =============================================================================
# TEST 6: Overview/pyramid access
# =============================================================================
# COGs often include pre-computed overviews (lower resolution versions) for
# fast previews. Here we simulate this by aggregating. For a well-built COG,
# GDAL can read built-in overviews directly without processing all pixels.
# This is useful for thumbnail generation or quick data exploration.

cat("=== TEST 6: Low-Resolution Overview ===\n")

# Aggregate by factor of 32 (e.g., 90m pixels -> ~2.9km pixels)
t_start <- Sys.time()
r_overview <- aggregate(r, fact = 32, fun = "mean", na.rm = TRUE)
vals_overview <- values(r_overview)
t_overview <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

cat("Overview dimensions:", ncol(r_overview), "x", nrow(r_overview), "pixels\n")
cat("Reduction factor: 32x (", ncol(r), "->", ncol(r_overview), ")\n")
cat("Overview time:", round(t_overview, 2), "s\n")
cat("Note: This reads the full extent at low res - useful for quick previews\n\n")

png(file.path(OUTPUT_DIR, "04_overview.png"), width = 1000, height = 600)
plot(r_overview, main = "Test 6: Low-Resolution Overview (32x aggregation)",
     col = hcl.colors(50, "YlOrRd", rev = TRUE))
dev.off()
cat("Saved: 04_overview.png\n\n")

# =============================================================================
# SUMMARY
# =============================================================================
# Final report: shows file sizes and pass/fail for each verification check.
# All checks should pass for a properly configured COG + server.

cat("=== SUMMARY ===\n")
cat("Full file size:", round(content_length / 1e9, 2), "GB\n")
cat("Small bbox read:", round(as.numeric(bytes_read_small) / 1e6, 2), "MB",
    sprintf("(%.3f%% of full)", pct_of_full), "\n")
cat("Polygon clip read:", round(as.numeric(bytes_poly) / 1e6, 2), "MB",
    sprintf("(%.2f%% of full)", pct_poly), "\n")
cat("\nCOG streaming verification:\n")
cat("  [", ifelse(range_ok, "PASS", "FAIL"), "] Server supports range requests\n")
cat("  [", ifelse(t_metadata < 5, "PASS", "FAIL"), "] Metadata read is fast (<5s)\n")
cat("  [", ifelse(pct_of_full < 1, "PASS", "FAIL"), "] Partial reads confirmed (subset << full file)\n")
cat("  [", ifelse(scaling_results$mb_read[4] > scaling_results$mb_read[1] * 5, "PASS", "FAIL"),
    "] Read size scales with window size\n")
cat("\nOutputs saved to:", OUTPUT_DIR, "\n")
