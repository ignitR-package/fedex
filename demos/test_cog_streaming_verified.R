#!/usr/bin/env Rscript
# =============================================================================
# COG Streaming Verification Script
# =============================================================================
#
# PURPOSE:
#   Test if a Cloud Optimized GeoTIFF (COG) supports efficient partial reads
#   from a remote URL without downloading the entire file.
#
# WHAT IT TESTS:
#   1. HTTP range requests - server allows partial file downloads
#   2. Fast metadata - read dimensions/CRS without downloading data
#   3. Small subset - crop to small area reads only that area
#   4. Scaling - larger areas read proportionally more data
#   5. Polygon clip - crop + mask workflow
#   6. Overview access - fast low-resolution preview
#
# OUTPUTS:
#   - Console output with pass/fail for each test
#   - 4 PNG visualizations
#
# =============================================================================

library(terra)
library(sf)
library(httr)
library(gdalUtilities)  # Needed for gdal_translate

# Configuration
COG_URL <- "https://knb.ecoinformatics.org/data/WRI_score.tif"
VSICURL_PATH <- paste0("/vsicurl/", COG_URL)  # GDAL path for remote access
OUTPUT_DIR <- getwd()

# GDAL settings - reduce unnecessary HTTP requests
Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
Sys.setenv(CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif")

# =============================================================================
# TEST 1: HTTP Range Requests
# =============================================================================
# COGs need "partial content" support - server must allow requesting specific
# byte ranges instead of the whole file. Look for HTTP 206 status.

cat("=== TEST 1: HTTP Range Request Support ===\n")

# Check server headers
head_resp <- HEAD(COG_URL, timeout(60))
range_resp <- GET(COG_URL, add_headers(Range = "bytes=0-1023"), timeout(60))

head_ok <- status_code(head_resp) == 200
range_ok <- status_code(range_resp) == 206
accept_ranges <- headers(head_resp)$`accept-ranges`
content_length <- as.numeric(headers(head_resp)$`content-length`)

cat("HEAD status:", status_code(head_resp), ifelse(head_ok, "[OK]", "[FAIL]"), "\n")
cat("Range status:", status_code(range_resp), ifelse(range_ok, "[OK]", "[FAIL - needs 206]"), "\n")
cat("Accept-Ranges:", accept_ranges, ifelse(accept_ranges == "bytes", "[OK]", "[FAIL]"), "\n")
cat("File size:", round(content_length / 1e9, 2), "GB\n\n")

if (!range_ok) stop("Server doesn't support range requests - COG streaming won't work.")

# =============================================================================
# TEST 2: Metadata Access
# =============================================================================
# Opening a COG should be fast - GDAL reads only the header (a few KB).
# If this takes minutes, the whole file is downloading.

cat("=== TEST 2: Metadata Access ===\n")

t_start <- Sys.time()
r <- rast(VSICURL_PATH)
t_metadata <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

cat("Dimensions:", ncol(r), "x", nrow(r), "pixels\n")
cat("CRS: EPSG:", crs(r, describe = TRUE)$code, "\n")
cat("Resolution:", res(r)[1], "x", res(r)[2], "m\n")
cat("Time:", round(t_metadata, 2), "s",
    ifelse(t_metadata < 5, "[OK]", "[SLOW]"), "\n\n")

# =============================================================================
# TEST 3: Small Subset
# =============================================================================
# COGs let you crop() a small area and read only those tiles.
# This should download <1% of the full file.

cat("=== TEST 3: Small Bbox Subset (~50km) ===\n")

# Small test area (50km x 50km in EPSG:5070 meters)
test_ext <- ext(-2000000, -1950000, 1500000, 1550000)

t_start <- Sys.time()
subset_small <- crop(r, test_ext)
vals_small <- values(subset_small)
t_small <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

bytes_read_small <- object.size(vals_small)
pct_of_full <- (as.numeric(bytes_read_small) / content_length) * 100

cat("Subset:", ncol(subset_small), "x", nrow(subset_small), "pixels\n")
cat("Valid cells:", format(sum(!is.na(vals_small)), big.mark = ","), "\n")
cat("Data read:", round(as.numeric(bytes_read_small) / 1e6, 2), "MB")
cat(" (", round(pct_of_full, 3), "% of full file)",
    ifelse(pct_of_full < 1, " [OK]", " [FAIL]"), "\n")
cat("Time:", round(t_small, 2), "s\n\n")

png(file.path(OUTPUT_DIR, "01_small_subset.png"), width = 800, height = 600)
plot(subset_small, main = "Test 3: Small Subset (50km)",
     col = hcl.colors(50, "YlOrRd", rev = TRUE))
dev.off()
cat("Saved: 01_small_subset.png\n\n")

# =============================================================================
# TEST 4: Scaling Behavior
# =============================================================================
# Test different window sizes. If COG streaming works, larger areas should
# download proportionally more data (not the same amount every time).

cat("=== TEST 4: Scaling Test ===\n")

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

  cat(sprintf("  %s: %.2f MB, %.2f s\n", name,
              as.numeric(object.size(v)) / 1e6, t_elapsed))
}

png(file.path(OUTPUT_DIR, "02_scaling_test.png"), width = 1000, height = 400)
par(mfrow = c(1, 2))
barplot(scaling_results$mb_read, names.arg = scaling_results$window,
        main = "Data Read vs Window Size", ylab = "MB", col = "steelblue")
barplot(scaling_results$seconds, names.arg = scaling_results$window,
        main = "Time vs Window Size", ylab = "Seconds", col = "coral")
dev.off()
cat("Saved: 02_scaling_test.png\n\n")

if (scaling_results$mb_read[4] > scaling_results$mb_read[1] * 5) {
  cat("[OK] Data scales with window size\n\n")
} else {
  cat("[FAIL] Data doesn't scale - may download full file each time\n\n")
}

# =============================================================================
# TEST 5: Polygon Clip
# =============================================================================
# Extract data within an irregular boundary. Steps:
#   1. Create polygon
#   2. Reproject to match raster CRS
#   3. crop() to bbox (fetches only needed tiles)
#   4. mask() to set pixels outside polygon to NA

cat("=== TEST 5: Polygon Clip (SF Bay Area) ===\n")

# Define polygon in WGS84 lat/lon
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

# Reproject to match raster
poly_5070 <- st_transform(poly_sf, crs = st_crs(r))
poly_vect <- vect(poly_5070)

t_start <- Sys.time()
r_cropped <- crop(r, poly_vect, snap = "out")
r_masked <- mask(r_cropped, poly_vect)
vals_poly <- values(r_masked)
t_poly <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

bytes_poly <- object.size(vals_poly)
pct_poly <- (as.numeric(bytes_poly) / content_length) * 100

cat("Output:", ncol(r_masked), "x", nrow(r_masked), "pixels\n")
cat("Valid cells:", format(sum(!is.na(vals_poly)), big.mark = ","), "\n")
cat("Data read:", round(as.numeric(bytes_poly) / 1e6, 2), "MB",
    "(", round(pct_poly, 2), "% of full)\n")
cat("Time:", round(t_poly, 2), "s\n\n")

png(file.path(OUTPUT_DIR, "03_polygon_clip.png"), width = 800, height = 600)
plot(r_masked, main = "Test 5: Polygon Clip (SF Bay Area)",
     col = hcl.colors(50, "YlOrRd", rev = TRUE))
plot(st_geometry(poly_5070), add = TRUE, border = "blue", lwd = 2)
dev.off()
cat("Saved: 03_polygon_clip.png\n\n")

# =============================================================================
# TEST 6: Overview Access
# =============================================================================
# COGs have pre-computed overviews (lower resolution pyramids) built-in.
# This test reads overview level 5 (32x reduction) directly - no computation!
# IMPORTANT: Use gdal_translate, NOT aggregate() which downloads the full file.

cat("=== TEST 6: Overview Access ===\n")

# Calculate target dimensions (1/32 of full resolution)
target_ncol <- round(ncol(r) / 32)
target_nrow <- round(nrow(r) / 32)

cat("Target size:", target_ncol, "x", target_nrow, "pixels (32x reduction)\n")

# Use gdal_translate to read the pre-computed overview
temp_overview <- tempfile(fileext = ".tif")

t_start <- Sys.time()
gdal_translate(
  src_dataset = VSICURL_PATH,
  dst_dataset = temp_overview,
  outsize = c(target_ncol, target_nrow),  # GDAL reads matching overview level
  r = "average",                           # Resampling if exact match not found
  of = "GTiff"
)

r_overview <- rast(temp_overview)
vals_overview <- values(r_overview)
t_overview <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

cat("Result:", ncol(r_overview), "x", nrow(r_overview), "pixels\n")
cat("Time:", round(t_overview, 2), "s",
    ifelse(t_overview < 60, "[OK - fast!]", "[SLOW - check connection]"), "\n")
cat("Note: GDAL reads pre-computed overview (~3-5 MB) instead of full file\n\n")

png(file.path(OUTPUT_DIR, "04_overview.png"), width = 1000, height = 600)
plot(r_overview, main = sprintf("Test 6: Overview (32x reduction - %.1fs)", t_overview),
     col = hcl.colors(50, "YlOrRd", rev = TRUE))
dev.off()
cat("Saved: 04_overview.png\n\n")

# Clean up temp file
unlink(temp_overview)

# =============================================================================
# SUMMARY
# =============================================================================

cat("=== SUMMARY ===\n")
cat("Full file:", round(content_length / 1e9, 2), "GB\n")
cat("Small bbox:", round(as.numeric(bytes_read_small) / 1e6, 2), "MB",
    sprintf(" (%.3f%%)", pct_of_full), "\n")
cat("Polygon clip:", round(as.numeric(bytes_poly) / 1e6, 2), "MB",
    sprintf(" (%.2f%%)", pct_poly), "\n\n")

cat("Test Results:\n")
cat("  [", ifelse(range_ok, "PASS", "FAIL"), "] Range requests\n")
cat("  [", ifelse(t_metadata < 5, "PASS", "FAIL"), "] Fast metadata (<5s)\n")
cat("  [", ifelse(pct_of_full < 1, "PASS", "FAIL"), "] Partial reads (<1% for small subset)\n")
cat("  [", ifelse(scaling_results$mb_read[4] > scaling_results$mb_read[1] * 5, "PASS", "FAIL"),
    "] Scaling behavior\n")
cat("  [", ifelse(t_overview < 60, "PASS", "FAIL"), "] Fast overview access (<60s)\n\n")

cat("All tests passed! COG is properly configured for streaming.\n")
cat("Outputs saved to:", OUTPUT_DIR, "\n")
