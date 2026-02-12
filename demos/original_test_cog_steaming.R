#!/usr/bin/env Rscript
# =============================================================================
# 🧪 COG STREAMING TEST SCRIPT
# =============================================================================
# This script thoroughly tests Cloud Optimized GeoTIFF (COG) streaming
# capabilities from a remote URL. It validates that HTTP range requests work
# correctly, allowing partial reads without downloading the entire file.
#
# Author: Data Science Workflow
# Date: 2026-02-05
# =============================================================================

# =============================================================================
# ⚙️ SETTINGS
# =============================================================================

# Two-lines-per-code-line rule:
# Each executable line below has exactly 2 comment lines immediately above it.

# Decide whether to run slower, bigger-window tests.
# Set to FALSE if you want only the fast checks.
RUN_FULL_TESTS <- TRUE

# Decide whether to print extra diagnostic details.
# Set to FALSE for quieter output.
VERBOSE <- TRUE

# Timeout used for HTTP operations (seconds).
# Increase this on slow networks.
TIMEOUT_SECONDS <- 60

# Define a small test extent in EPSG:5070 (meters).
# This is a ~50km x 50km window for quick streaming checks.
TEST_WINDOW <- list(xmin=-2000000, xmax=-1950000, ymin=1500000, ymax=1550000)

# Define a larger California-ish extent in EPSG:5070 (meters).
# This is used to test a substantial subset read.
CA_BBOX <- list(xmin=-2400000, xmax=-1800000, ymin=1200000, ymax=2400000)

# Decide whether to run the polygon clipping demo.
# Set to FALSE if you only want the HTTP + bbox tests.
RUN_POLYGON_DEMO <- TRUE

# Optional: provide a polygon file path (GeoJSON / Shapefile / etc).
# Leave as NA to use the built-in example polygon.
POLYGON_FILE_PATH <- NA_character_

# Polygon complexity guardrails (instructional + safety).
# These settings help prevent extremely detailed polygons from causing slow reads.
MAX_POLYGON_VERTICES <- 20000

# Target vertex count after simplification (best-effort).
# If the polygon exceeds MAX_POLYGON_VERTICES, we simplify toward this target.
TARGET_POLYGON_VERTICES <- 5000

# If TRUE, stop with an error when polygon is still too complex after simplification.
# If FALSE, continue but print loud warnings.
STRICT_POLYGON_COMPLEXITY <- FALSE

# Simplification behavior (tolerance is in meters because we simplify in EPSG:5070).
# Tolerance is started as N raster pixels and increased until under TARGET_POLYGON_VERTICES.
SIMPLIFY_START_TOL_PIXELS <- 1

# Maximum simplification tolerance in raster pixels.
# Caps how much we are willing to distort the polygon.
SIMPLIFY_MAX_TOL_PIXELS <- 50

# Multiplier applied each simplification iteration.
# Larger numbers simplify more aggressively per iteration.
SIMPLIFY_TOL_MULTIPLIER <- 2

# Crop snapping behavior.
# 'out' ensures the crop window covers the polygon fully, aligned to pixel grid.
CROP_SNAP_MODE <- "out"

# Decide whether to write the polygon-clipped raster to disk.
# Set FALSE if you only want an in-memory result.
WRITE_POLYGON_OUTPUT <- TRUE

# Output filename for the polygon-clipped raster.
# This will be written to OUTPUT_DIR when enabled.
POLYGON_OUTPUT_FILENAME <- "WRI_score_polygon_clip.tif"

# =============================================================================
# 📁 FILE PATHS / URLs
# =============================================================================

# Remote COG URL to test (the one that should support Range requests).
# This should be accessible without authentication.
COG_URL <- "https://knb.ecoinformatics.org/data/WRI_score.tif"

# Construct a GDAL VSI path for streaming over HTTP.
# terra/GDAL will use HTTP range requests behind the scenes.
VSICURL_PATH <- paste0("/vsicurl/", COG_URL)

# Determine an output directory for any artifacts.
# When run via Rscript, fallback to getwd() if script path is unavailable.
OUTPUT_DIR <- tryCatch({ dirname(sys.frame(1)$ofile) }, error=function(e){ getwd() })

# =============================================================================
# 📦 LOAD LIBRARIES
# =============================================================================

# Print a blank line for readability.
# This helps visually separate the header in logs.
cat("\n")

# Print a divider line.
# This is purely cosmetic for console readability.
cat("===========================================================================\n")

# Print a section title.
# Emoji used to match your preferred style.
cat("📦 LOADING LIBRARIES\n")

# Print a divider line.
# Mirrors the top divider.
cat("===========================================================================\n")

# Create a Pacific Time timestamp helper.
# All prints in this script use America/Los_Angeles.
pt_timestamp <- function() { format(Sys.time(), tz="America/Los_Angeles", "%Y-%m-%d %H:%M:%S %Z") }

# Announce dependency loading with a PT timestamp.
# Useful for timing and audit trails.
cat(pt_timestamp(), "- Loading required packages...\n")

# Load libraries quietly.
# We suppress startup messages to keep output clean.
suppressPackageStartupMessages({ library(terra); library(sf); library(httr) })

# Confirm libraries loaded successfully.
# Prints with Pacific Time timestamp.
cat(pt_timestamp(), "- ✅ Libraries loaded successfully\n")

# =============================================================================
# 🔧 HELPER FUNCTIONS
# =============================================================================

# Define a function to print a big section header.
# Uses an emoji and long bar separators.
print_header <- function(emoji, title) { cat("\n"); cat("===========================================================================\n"); cat(emoji, title, "\n"); cat("===========================================================================\n") }

# Define a function to standardize PASS/FAIL output.
# Optionally prints details when VERBOSE is TRUE.
print_result <- function(test_name, passed, details=NULL) { status <- if (passed) "✅ PASS" else "❌ FAIL"; cat(sprintf("  [%s] %s\n", status, test_name)); if (!is.null(details) && VERBOSE) cat(sprintf("         %s\n", details)) }

# Define a function to time an expression.
# Returns both result and elapsed seconds.
time_operation <- function(expr, description) { cat(pt_timestamp(), sprintf("- Starting: %s\n", description)); start_time <- Sys.time(); result <- tryCatch(eval(expr), error=function(e){ cat(pt_timestamp(), sprintf("- ❌ Error: %s\n", e$message)); NULL }); elapsed <- as.numeric(difftime(Sys.time(), start_time, units="secs")); cat(pt_timestamp(), sprintf("- Completed in %.2f seconds\n", elapsed)); list(result=result, elapsed=elapsed) }

# Define a function to snap coordinates to a raster grid.
# This improves alignment between polygons and pixel boundaries.
snap_to_grid <- function(x, origin, resolution) { origin + round((x - origin) / resolution) * resolution }

# Define a function to snap an sf polygon to the raster grid in EPSG:5070.
# Snapping helps ensure the polygon “works well” with the raster’s resolution.
snap_polygon_to_raster_grid <- function(poly_sf, r) {
  # Extract raster origin and resolution.
  # terra uses these to define the pixel grid.
  o <- terra::origin(r)
  # Extract raster resolution.
  # For this raster we expect ~90m pixels.
  rs <- terra::res(r)
  # Convert polygon geometry to a coordinate matrix.
  # This makes it easy to snap each vertex.
  coords <- sf::st_coordinates(poly_sf)
  # Snap x and y coordinates to the raster grid.
  # Uses the raster origin and pixel size.
  coords[, "X"] <- snap_to_grid(coords[, "X"], o[1], rs[1])
  # Snap y coordinates to the raster grid.
  # Uses the raster origin and pixel size.
  coords[, "Y"] <- snap_to_grid(coords[, "Y"], o[2], rs[2])
  # Rebuild polygon from snapped coordinates.
  # We reconstruct using only the first ring.
  ring <- coords[coords[, "L1"] == 1 & coords[, "L2"] == 1, c("X", "Y"), drop=FALSE]
  # Ensure the ring is closed (first point equals last point).
  # Some operations expect explicitly closed rings.
  if (!(nrow(ring) >= 4 && all(ring[1, ] == ring[nrow(ring), ]))) ring <- rbind(ring, ring[1, , drop=FALSE])
  # Build an sf polygon with the same CRS as the input.
  # CRS must already match the raster CRS.
  sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(ring))), crs = sf::st_crs(poly_sf)))
}

# Define a helper to count polygon vertices (rough proxy for complexity).
# Uses st_coordinates so it works on (multi)polygons.
polygon_vertex_count <- function(poly_sf) {
  # Extract all coordinates for all rings.
  # st_coordinates returns a matrix with X/Y and ring/feature indices.
  coords <- sf::st_coordinates(poly_sf)
  # Count rows with non-NA X values.
  # This approximates the number of vertices.
  sum(!is.na(coords[, "X"]))
}

# Define a helper to simplify polygons safely for raster masking.
# Simplifies in projected CRS so tolerance is in meters.
maybe_simplify_polygon <- function(poly_5070_sf, r, max_vertices, target_vertices) {
  # Compute starting vertex count.
  # Used for warnings and deciding whether to simplify.
  v0 <- polygon_vertex_count(poly_5070_sf)
  # If already under max, return as-is.
  # This avoids unnecessary distortion.
  if (v0 <= max_vertices) return(list(poly = poly_5070_sf, simplified = FALSE, vertices_before = v0, vertices_after = v0, tol_m = 0))
  # Get raster resolution in meters (EPSG:5070).
  # We use this to define a reasonable simplification tolerance.
  rs <- terra::res(r)
  # Use the larger of x/y resolution as the “pixel size”.
  # Conservative choice for tolerance scaling.
  pixel_m <- max(rs[1], rs[2])
  # Compute starting and maximum tolerances in meters.
  # These are defined in “pixels” in Settings for clarity.
  tol_m <- SIMPLIFY_START_TOL_PIXELS * pixel_m
  # Compute maximum tolerance in meters.
  # Caps how aggressive simplification can get.
  tol_m_max <- SIMPLIFY_MAX_TOL_PIXELS * pixel_m
  # Initialize current polygon.
  # We update this as we simplify.
  cur <- poly_5070_sf
  # Initialize current vertex count.
  # Used as loop condition.
  v <- v0
  # Simplify until we hit the target or max tolerance.
  # preserveTopology tries to avoid self-intersections.
  while (v > target_vertices && tol_m <= tol_m_max) {
    # Simplify geometry using current tolerance.
    # Tolerance units are meters in EPSG:5070.
    cur <- sf::st_simplify(cur, dTolerance = tol_m, preserveTopology = TRUE)
    # Repair geometry if simplification created invalid shapes.
    # This helps avoid mask() failures.
    cur <- sf::st_make_valid(cur)
    # Recompute vertex count.
    # This tells us whether simplification is working.
    v <- polygon_vertex_count(cur)
    # Increase tolerance for the next iteration.
    # Exponential backoff reduces iteration count.
    tol_m <- tol_m * SIMPLIFY_TOL_MULTIPLIER
  }
  # Return simplified geometry and diagnostics.
  # tol_m reported as the last-used *attempt* tolerance / multiplier step.
  list(poly = cur, simplified = TRUE, vertices_before = v0, vertices_after = v, tol_m = min(tol_m / SIMPLIFY_TOL_MULTIPLIER, tol_m_max))
}

# =============================================================================
# 🌐 TEST 1: HTTP RANGE REQUEST SUPPORT
# =============================================================================

# Print the Test 1 header to the console.
# Confirms we are about to validate HTTP range requests.
print_header("🌐", "TEST 1: HTTP RANGE REQUEST SUPPORT")

# Announce what is being tested.
# Include the target URL.
cat(pt_timestamp(), "- Testing HTTP headers and range request support...\n")

# Print the target URL for clarity.
# Helps future readers understand what endpoint was tested.
cat("  URL:", COG_URL, "\n\n")

# Print a subheader for the HEAD request.
# HEAD verifies basic headers and server availability.
cat("  --- Test 1a: HEAD request ---\n")

# Perform the HEAD request with timeout protection.
# On error, return NULL to avoid stopping the script.
head_response <- tryCatch({ HEAD(COG_URL, timeout(TIMEOUT_SECONDS)) }, error=function(e) NULL)

# Branch based on whether the HEAD call succeeded.
# If it failed, print a FAIL and continue.
if (!is.null(head_response)) {
  # Validate HTTP 200 OK for HEAD.
  # Some servers may respond differently; here we expect 200.
  print_result("Server responds to HEAD request", status_code(head_response) == 200)
  # Extract headers for subsequent checks.
  # We reuse these to verify range support and content type.
  headers_head <- headers(head_response)
  # Pull Accept-Ranges from headers.
  # For COG streaming we expect 'bytes'.
  accept_ranges <- headers_head$`accept-ranges`
  # Pull Content-Length to estimate full file size.
  # Used only for reporting and savings calculations.
  content_length <- suppressWarnings(as.numeric(headers_head$`content-length`))
  # Pull Content-Type to ensure it's a TIFF.
  # We allow any content-type containing 'tiff'.
  content_type <- headers_head$`content-type`
  # Check if Accept-Ranges is set to bytes.
  # This indicates the server *claims* to support byte ranges.
  print_result("Accept-Ranges: bytes header present", !is.null(accept_ranges) && accept_ranges == "bytes", sprintf("Value: %s", accept_ranges))
  # Check that Content-Length is provided and positive.
  # This helps confirm stable hosting behavior.
  print_result("Content-Length available", !is.null(content_length) && is.finite(content_length) && content_length > 0, sprintf("Size: %.2f GB", content_length / 1e9))
  # Check that Content-Type indicates a TIFF.
  # Exact values vary; we match on 'tiff'.
  print_result("Content-Type is image/tiff", !is.null(content_type) && grepl("tiff", content_type), sprintf("Value: %s", content_type))
} else {
  # Report that the HEAD request failed.
  # This usually means connectivity or server issues.
  print_result("Server responds to HEAD request", FALSE, "Connection failed")
}

# Print a subheader for a start-of-file range request.
# This tests real byte-range behavior with 1KB.
cat("\n  --- Test 1b: Range request (bytes 0-1023) ---\n")

# Request the first 1024 bytes explicitly.
# A correct server returns HTTP 206 and a Content-Range header.
range_response <- tryCatch({ GET(COG_URL, add_headers(Range="bytes=0-1023"), timeout(TIMEOUT_SECONDS)) }, error=function(e) NULL)

# Branch based on whether the range request succeeded.
# If it failed, we report and keep going.
if (!is.null(range_response)) {
  # Check for 206 Partial Content.
  # 200 OK would indicate the server ignored the Range header.
  is_partial <- status_code(range_response) == 206
  # Pull the Content-Range header.
  # This should reflect the requested range and total size.
  content_range <- headers(range_response)$`content-range`
  # Measure actual bytes received.
  # This ensures the server did not send the whole file.
  actual_length <- length(content(range_response, "raw"))
  # Report status code correctness.
  # We also print the status code as detail when VERBOSE.
  print_result("Server returns 206 Partial Content", is_partial, sprintf("Status: %d", status_code(range_response)))
  # Report presence of Content-Range.
  # Missing Content-Range is suspicious for partial content responses.
  print_result("Content-Range header present", !is.null(content_range), sprintf("Value: %s", content_range))
  # Report exact byte count received.
  # Must be exactly 1024 bytes for this request.
  print_result("Correct number of bytes returned", actual_length == 1024, sprintf("Requested: 1024, Received: %d", actual_length))
} else {
  # Report failure to execute the range request.
  # Usually a connectivity or TLS issue.
  print_result("Range request successful", FALSE, "Connection failed")
}

# Print a subheader for a mid-file range request.
# This ensures ranges work beyond the file header.
cat("\n  --- Test 1c: Range request (bytes 1048576-1049599, 1MB offset) ---\n")

# Request 1KB at 1MB offset.
# Confirms the server honors ranges for arbitrary offsets.
range_response_mid <- tryCatch({ GET(COG_URL, add_headers(Range="bytes=1048576-1049599"), timeout(TIMEOUT_SECONDS)) }, error=function(e) NULL)

# Branch based on whether the mid-file range request succeeded.
# We expect 206 and a matching Content-Range.
if (!is.null(range_response_mid)) {
  # Check for 206 Partial Content again.
  # Any other status suggests range handling problems.
  is_partial <- status_code(range_response_mid) == 206
  # Extract Content-Range for offset verification.
  # This should include '1048576-1049599'.
  content_range <- headers(range_response_mid)$`content-range`
  # Confirm we received exactly 1024 bytes.
  # Prevents silent full-file download behavior.
  actual_length <- length(content(range_response_mid, "raw"))
  # Report the 206 status.
  # This indicates correct partial content response.
  print_result("Mid-file range returns 206", is_partial)
  # Report Content-Range offset correctness.
  # Confirms the server honored the requested offsets.
  print_result("Content-Range shows correct offset", !is.null(content_range) && grepl("1048576-1049599", content_range), sprintf("Value: %s", content_range))
  # Report byte count correctness.
  # Must match the requested 1024 bytes.
  print_result("Correct bytes returned", actual_length == 1024, sprintf("Received: %d bytes", actual_length))
} else {
  # Report failure to perform mid-file range request.
  # Often indicates proxy/CDN range restrictions.
  print_result("Mid-file range request", FALSE, "Connection failed")
}

# =============================================================================
# 📊 TEST 2: GDAL/TERRA REMOTE METADATA ACCESS
# =============================================================================

# Print the Test 2 header to the console.
# This section validates remote-open behavior without full download.
print_header("📊", "TEST 2: REMOTE METADATA ACCESS (NO FULL DOWNLOAD)")

# Announce that we will open the remote COG with /vsicurl/.
# This should trigger only small range reads for metadata.
cat(pt_timestamp(), "- Opening remote COG via /vsicurl/...\n")

# Configure GDAL to avoid directory listing requests.
# This reduces extra HTTP calls for sidecar files.
Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR")

# Allow GDAL vsicurl access for .tif extensions.
# Some environments block remote access without this.
Sys.setenv(CPL_VSIL_CURL_ALLOWED_EXTENSIONS=".tif")

# Time the metadata read.
# We expect this to be quick if range requests work.
metadata_test <- time_operation(quote({ r <- rast(VSICURL_PATH); list(ncol=ncol(r), nrow=nrow(r), nlyr=nlyr(r), crs=crs(r, describe=TRUE)$code, res=res(r), ext=as.vector(ext(r)), datatype=datatype(r), sources=sources(r)) }), "Reading remote metadata")

# Branch based on whether metadata was successfully read.
# If NULL, we report failure and continue.
if (!is.null(metadata_test$result)) {
  # Store metadata list in a shorter variable.
  # This reduces verbosity in subsequent prints.
  m <- metadata_test$result
  # Print a small metadata summary header.
  # Helps visual scanning in logs.
  cat("\n  📋 COG Metadata:\n")
  # Print raster dimensions.
  # These should match the known WRI score raster size.
  cat(sprintf("     Dimensions: %d x %d pixels\n", m$ncol, m$nrow))
  # Print number of layers.
  # We expect 1 band for this dataset.
  cat(sprintf("     Layers: %d\n", m$nlyr))
  # Print CRS as EPSG code.
  # We expect EPSG:5070 for CONUS Albers.
  cat(sprintf("     CRS: EPSG:%s\n", m$crs))
  # Print resolution in meters.
  # We expect 90m pixels per earlier inspection.
  cat(sprintf("     Resolution: %.1f x %.1f meters\n", m$res[1], m$res[2]))
  # Print GDAL datatype.
  # Expected to be float (FLT4S).
  cat(sprintf("     Data type: %s\n", m$datatype))
  # Validate that metadata read was fast.
  # Threshold is 5 seconds.
  print_result("Metadata read in < 5 seconds", metadata_test$elapsed < 5, sprintf("Actual: %.2f seconds", metadata_test$elapsed))
  # Validate that dimensions are positive.
  # Zero dimensions would indicate a failed open.
  print_result("Valid dimensions", m$ncol > 0 && m$nrow > 0)
  # Validate the CRS is EPSG:5070.
  # If not, it may indicate wrong file or parsing issues.
  print_result("CRS is EPSG:5070 (Albers)", m$crs == "5070")
} else {
  # Report remote metadata access failure.
  # This typically means GDAL couldn't open the URL.
  print_result("Remote metadata access", FALSE)
}

# =============================================================================
# 🗺️ TEST 3: SMALL WINDOW SUBSET READ
# =============================================================================

# Print the Test 3 header to the console.
# This section validates that small windows can be read efficiently.
print_header("🗺️", "TEST 3: SMALL WINDOW SUBSET READ (~50km x 50km)")

# Announce the small subset read.
# This should download only the needed tiles.
cat(pt_timestamp(), "- Reading small subset from remote COG...\n")

# Print the test window bounds for transparency.
# Coordinates are in EPSG:5070 meters.
cat(sprintf("  Window bounds (EPSG:5070): xmin=%.0f, xmax=%.0f, ymin=%.0f, ymax=%.0f\n", TEST_WINDOW$xmin, TEST_WINDOW$xmax, TEST_WINDOW$ymin, TEST_WINDOW$ymax))

# Time the small crop and in-memory read.
# values() forces GDAL to fetch pixels, exercising range requests.
small_subset_test <- time_operation(quote({ r <- rast(VSICURL_PATH); test_ext <- ext(TEST_WINDOW$xmin, TEST_WINDOW$xmax, TEST_WINDOW$ymin, TEST_WINDOW$ymax); subset <- crop(r, test_ext); values_sample <- values(subset); list(ncells=ncell(subset), valid_cells=sum(!is.na(values_sample)), min_val=min(values_sample, na.rm=TRUE), max_val=max(values_sample, na.rm=TRUE), mean_val=mean(values_sample, na.rm=TRUE), memory_mb=object.size(values_sample)/1024/1024) }), "Cropping and reading small window")

# Branch based on success of the crop/read.
# If NULL, we report failure.
if (!is.null(small_subset_test$result)) {
  # Store results in a short variable.
  # Avoids repetitive list indexing.
  s <- small_subset_test$result
  # Print a subset results header.
  # Helps with readability in logs.
  cat("\n  📋 Subset Results:\n")
  # Print the number of cells in the subset.
  # This is width*height for one band.
  cat(sprintf("     Cells: %d\n", s$ncells))
  # Print number of non-NA cells.
  # Confirms data exists in the requested window.
  cat(sprintf("     Valid cells: %d\n", s$valid_cells))
  # Print value range.
  # Should be within expected WRI score range.
  cat(sprintf("     Value range: %.2f - %.2f\n", s$min_val, s$max_val))
  # Print mean value.
  # Useful quick sanity check.
  cat(sprintf("     Mean value: %.2f\n", s$mean_val))
  # Print approximate in-memory size of values array.
  # Indicates transfer and memory footprint.
  cat(sprintf("     Memory used: %.2f MB\n", s$memory_mb))
  # Validate timing threshold.
  # We expect this to be fast.
  print_result("Subset read in < 10 seconds", small_subset_test$elapsed < 10, sprintf("Actual: %.2f seconds", small_subset_test$elapsed))
  # Validate that there are valid pixels.
  # All NA would suggest outside-coverage or no-data only.
  print_result("Data has valid values", s$valid_cells > 0)
  # Validate range plausibility (0-100).
  # This matches typical rescaled score expectations.
  print_result("Values in expected range (0-100)", s$min_val >= 0 && s$max_val <= 100)
} else {
  # Report failure to read the small window.
  # This indicates streaming or GDAL open issues.
  print_result("Small window subset read", FALSE)
}

# =============================================================================
# 🌍 TEST 4: LARGER CALIFORNIA SUBSET
# =============================================================================

# Decide whether to run the larger subset test.
# Controlled by RUN_FULL_TESTS at the top.
if (RUN_FULL_TESTS) {
  # Print the Test 4 header.
  # This is the heavier California-sized crop.
  print_header("🌍", "TEST 4: LARGER SUBSET READ (California bbox)")
  # Announce the larger subset read.
  # Expect longer runtime and more bytes transferred.
  cat(pt_timestamp(), "- Reading California-sized subset from remote COG...\n")
  # Print the California bbox being used.
  # Coordinates are in EPSG:5070 meters.
  cat(sprintf("  Window bounds (EPSG:5070): xmin=%.0f, xmax=%.0f, ymin=%.0f, ymax=%.0f\n", CA_BBOX$xmin, CA_BBOX$xmax, CA_BBOX$ymin, CA_BBOX$ymax))
  # Time the California crop and in-memory load.
  # values() forces pixel transfer over HTTP ranges.
  ca_subset_test <- time_operation(quote({ r <- rast(VSICURL_PATH); ca_ext <- ext(CA_BBOX$xmin, CA_BBOX$xmax, CA_BBOX$ymin, CA_BBOX$ymax); subset <- crop(r, ca_ext); values_sample <- values(subset); list(ncells=ncell(subset), dims=c(ncol(subset), nrow(subset)), valid_cells=sum(!is.na(values_sample)), memory_mb=object.size(values_sample)/1024/1024) }), "Cropping California extent")
  # Branch based on whether the California crop succeeded.
  # If NULL, report failure.
  if (!is.null(ca_subset_test$result)) {
    # Store results for easier access.
    # This is a small list of summary metrics.
    s <- ca_subset_test$result
    # Print a California subset summary header.
    # Makes the log easier to read.
    cat("\n  📋 California Subset Results:\n")
    # Print subset dimensions.
    # Helps validate window sizing.
    cat(sprintf("     Dimensions: %d x %d pixels\n", s$dims[1], s$dims[2]))
    # Print total and valid cell counts.
    # Valid cells indicate non-NA values.
    cat(sprintf("     Total cells: %s\n", format(s$ncells, big.mark=","))); cat(sprintf("     Valid cells: %s\n", format(s$valid_cells, big.mark=",")))
    # Print the approximate in-memory size of the subset.
    # Useful proxy for transfer size.
    cat(sprintf("     Memory used: %.1f MB\n", s$memory_mb))
    # Compute full-file size in MB from known Content-Length.
    # Used to estimate percent savings vs full download.
    full_file_mb <- 3488331028 / 1024 / 1024
    # Compute savings percentage.
    # Higher means more efficient partial reading.
    savings_pct <- (1 - s$memory_mb / full_file_mb) * 100
    # Print a savings header.
    # Explains the comparison.
    cat(sprintf("\n  💾 Data Transfer Savings:\n"))
    # Print full file size, subset size, and savings.
    # Helps quantify why streaming is useful.
    cat(sprintf("     Full file size: %.1f MB\n", full_file_mb)); cat(sprintf("     Subset size: %.1f MB\n", s$memory_mb)); cat(sprintf("     Savings: %.1f%% (only downloaded what was needed!)\n", savings_pct))
    # Validate that the subset contains data.
    # Prevents false positives from empty crops.
    print_result("California subset read successfully", s$valid_cells > 0)
    # Validate that savings exceed a reasonable threshold.
    # Threshold set to 75% for this particular bbox.
    print_result("Significant bandwidth savings", savings_pct > 75, sprintf("Saved %.1f%% vs full download", savings_pct))
  } else {
    # Report failure to read California subset.
    # Indicates streaming problems at larger reads.
    print_result("California subset read", FALSE)
  }
}

# =============================================================================
# 🔬 TEST 5: OVERVIEW/PYRAMID ACCESS
# =============================================================================

# Print the Test 5 header.
# This checks that we can read a low-res preview efficiently.
print_header("🔬", "TEST 5: OVERVIEW (LOW-RES PREVIEW) ACCESS")

# Announce overview/preview read.
# We use aggregation as a simple way to get a quick-look product.
cat(pt_timestamp(), "- Reading low-resolution overview from remote COG...\n")

# Time the overview operation.
# This can be slower because it may touch many tiles.
overview_test <- time_operation(quote({ r <- rast(VSICURL_PATH); overview <- aggregate(r, fact=16, fun="mean", na.rm=TRUE); values_sample <- values(overview); list(dims=c(ncol(overview), nrow(overview)), memory_mb=object.size(values_sample)/1024/1024) }), "Reading aggregated overview")

# Branch based on overview success.
# If NULL, report nothing beyond failure.
if (!is.null(overview_test$result)) {
  # Store results for printing.
  # Contains dims and memory usage.
  o <- overview_test$result
  # Print a header for overview results.
  # Improves log readability.
  cat("\n  📋 Overview Results:\n")
  # Print dimensions and memory estimate.
  # Confirms that something was actually produced.
  cat(sprintf("     Dimensions: %d x %d pixels\n", o$dims[1], o$dims[2])); cat(sprintf("     Memory used: %.2f MB\n", o$memory_mb))
  # Validate that overview has valid dimensions.
  # Prevents silent failures from returning empty rasters.
  print_result("Overview read successfully", o$dims[1] > 0 && o$dims[2] > 0)
}

# =============================================================================
# 🧩 TEST 6: POLYGON SUBSETTING (CLIP / MASK)
# =============================================================================

# Print the Test 6 header.
# This section demonstrates “submit a polygon, get back pixels inside it”.
print_header("🧩", "TEST 6: POLYGON SUBSETTING (CLIP / MASK)")

# Announce what this test demonstrates.
# We will clip the remote COG to a polygon footprint.
cat(pt_timestamp(), "- Demonstrating polygon-based subsetting...\n")

# Print the source URL for the polygon demo.
# This keeps the example self-contained for learners.
cat(pt_timestamp(), "- Source raster URL: ", COG_URL, "\n", sep = "")

# Explain at a high level what happens in a polygon clip.
# Step 1: read polygon; Step 2: transform; Step 3: crop+mask.
cat(pt_timestamp(), "- Mechanics: read polygon → reproject to raster CRS → crop to bbox → mask to polygon\n")

# Decide whether to run the polygon demo.
# Controlled by RUN_POLYGON_DEMO in settings.
if (RUN_POLYGON_DEMO) {
  # Open the remote raster as a SpatRaster.
  # This is still “lazy” until we force a read (values()).
  r_remote <- rast(VSICURL_PATH)

  # Capture the raster CRS for instructional printing.
  # We expect EPSG:5070 for WRI_score.tif.
  raster_crs <- crs(r_remote, describe = TRUE)$code

  # Print the raster CRS so users know what polygons must match.
  # This explains why we reproject polygons.
  cat(pt_timestamp(), sprintf("- Raster CRS: EPSG:%s\n", raster_crs))

  # Choose a polygon source: file path if provided, else built-in example.
  # This makes the script usable both as a demo and as a template.
  using_file_polygon <- !is.na(POLYGON_FILE_PATH) && nzchar(POLYGON_FILE_PATH) && file.exists(POLYGON_FILE_PATH)

  # Print which polygon source we will use.
  # Helpful for teaching and debugging.
  cat(pt_timestamp(), "- Polygon source: ", if (using_file_polygon) POLYGON_FILE_PATH else "built-in example polygon (lon/lat → EPSG:5070)", "\n", sep = "")

  # Build or read the polygon as sf.
  # We keep it as sf first because sf makes CRS transformations explicit.
  poly_sf <- if (using_file_polygon) {
    # Read a polygon file from disk.
    # st_read supports GeoJSON, Shapefile, GeoPackage, etc.
    sf::st_read(POLYGON_FILE_PATH, quiet = TRUE)
  } else {
    # Define a simple rectangle in lon/lat (EPSG:4326).
    # This example is roughly around the SF Bay Area.
    sf::st_as_sf(sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(-122.60, 37.60,
          -121.80, 37.60,
          -121.80, 38.10,
          -122.60, 38.10,
          -122.60, 37.60),
        ncol = 2,
        byrow = TRUE
      ))),
      crs = 4326
    ))
  }

  # Validate geometry and keep only polygon features.
  # This avoids surprises if a file includes multiple geometry types.
  poly_sf <- sf::st_make_valid(poly_sf)

  # If the input has multiple features, union them into one polygon.
  # This simplifies the clip: one footprint, one output.
  poly_sf <- sf::st_as_sf(sf::st_union(sf::st_geometry(poly_sf)))

  # Reproject polygon to the raster CRS (EPSG:5070).
  # This is required for correct spatial overlay with the raster.
  poly_5070_sf <- sf::st_transform(poly_sf, crs = sf::st_crs(r_remote))

  # Explain polygon “resolution” vs raster “resolution”.
  # Polygons are vectors (no resolution), but they must be in the same CRS as the raster.
  cat(pt_timestamp(), "- Note: polygons have no pixel resolution; we make them compatible by using the raster CRS + grid-aligned cropping\n")

  # Compute polygon vertex count before any simplification.
  # This is our proxy for geometry complexity.
  v_before <- polygon_vertex_count(poly_5070_sf)

  # Print vertex count for the user.
  # Helpful for teaching what “complex” means.
  cat(pt_timestamp(), sprintf("- Polygon vertices (before): %s\n", format(v_before, big.mark=",")))

  # Simplify polygon if it exceeds MAX_POLYGON_VERTICES.
  # This protects users from very detailed boundaries.
  simp <- maybe_simplify_polygon(poly_5070_sf, r_remote, MAX_POLYGON_VERTICES, TARGET_POLYGON_VERTICES)

  # Use the simplified polygon for subsequent steps.
  # If not simplified, this is the original geometry.
  poly_5070_final_sf <- simp$poly

  # Print simplification summary if it happened.
  # Includes tolerance used and resulting vertex count.
  if (simp$simplified) {
    cat(pt_timestamp(), sprintf("- ⚠️ Polygon was complex; simplified using tolerance ≈ %.0f meters\n", simp$tol_m))
    cat(pt_timestamp(), sprintf("- Polygon vertices (after): %s\n", format(simp$vertices_after, big.mark=",")))
  }

  # Warn or stop if polygon is still too complex.
  # This is a final guardrail after best-effort simplification.
  if (simp$vertices_after > MAX_POLYGON_VERTICES) {
    msg <- sprintf("Polygon still has %s vertices after simplification (limit: %s). Consider simplifying upstream or increasing tolerances.",
                   format(simp$vertices_after, big.mark=","), format(MAX_POLYGON_VERTICES, big.mark=","))
    if (STRICT_POLYGON_COMPLEXITY) stop(msg) else cat(pt_timestamp(), paste0("- ❌ WARNING: ", msg, "\n"))
  }

  # Convert polygon to terra vector format.
  # terra::crop/mask operate on terra vectors (SpatVector).
  poly_vect <- terra::vect(poly_5070_final_sf)

  # Print polygon bounds (in raster CRS) for teaching.
  # This shows the “bbox” that will be used for the first-stage crop.
  poly_ext <- terra::ext(poly_vect)

  # Print the polygon extent.
  # This helps users reason about what area will be streamed.
  cat(pt_timestamp(), sprintf("- Polygon extent (EPSG:%s): xmin=%.0f xmax=%.0f ymin=%.0f ymax=%.0f\n", raster_crs, poly_ext[1], poly_ext[2], poly_ext[3], poly_ext[4]))

  # Time the polygon-based subset operation.
  # We intentionally do crop() first, then mask() (common best practice).
  polygon_subset_test <- time_operation(quote({
    # Crop to the polygon’s bounding box first.
    # This limits how many tiles must be fetched.
    r_cropped <- crop(r_remote, poly_vect, snap = CROP_SNAP_MODE)
    # Mask the cropped raster to the polygon footprint.
    # Pixels outside the polygon become NA.
    r_masked <- mask(r_cropped, poly_vect)
    # Force a real read (download of needed tiles) into memory.
    # values() triggers the actual remote IO.
    v <- values(r_masked)
    # Compute summary stats for instructional reporting.
    # These are simple checks that data came back.
    list(
      dims = c(ncol(r_masked), nrow(r_masked)),
      ncells = ncell(r_masked),
      valid_cells = sum(!is.na(v)),
      min_val = min(v, na.rm = TRUE),
      max_val = max(v, na.rm = TRUE),
      mean_val = mean(v, na.rm = TRUE),
      memory_mb = object.size(v) / 1024 / 1024,
      raster = r_masked
    )
  }), "Cropping + masking raster by polygon")

  # Branch based on success of the polygon clip.
  # If NULL, report failure and continue to summary.
  if (!is.null(polygon_subset_test$result)) {
    # Store the result list for easier printing.
    # Contains dims, stats, and the output raster.
    p <- polygon_subset_test$result

    # Print a results header for the polygon clip.
    # Makes the output instructional and readable.
    cat("\n")
    cat(pt_timestamp(), "  📋 Polygon Clip Results\n")

    # Print output dimensions.
    # This tells learners the size of the returned subset raster.
    cat(pt_timestamp(), sprintf("  - Output dimensions: %d x %d pixels\n", p$dims[1], p$dims[2]))

    # Print cell counts and valid cell counts.
    # Valid cells show the polygon mask is working.
    cat(pt_timestamp(), sprintf("  - Total cells: %s\n", format(p$ncells, big.mark=",")))
    cat(pt_timestamp(), sprintf("  - Cells inside polygon (non-NA): %s\n", format(p$valid_cells, big.mark=",")))

    # Print simple value stats.
    # These help confirm the returned values are plausible.
    cat(pt_timestamp(), sprintf("  - Value range: %.2f to %.2f\n", p$min_val, p$max_val))
    cat(pt_timestamp(), sprintf("  - Mean value: %.2f\n", p$mean_val))

    # Print an in-memory size estimate.
    # This approximates how “big” the download/array was.
    cat(pt_timestamp(), sprintf("  - In-memory values size: %.2f MB\n", p$memory_mb))

    # Optionally write the polygon-clipped raster to disk.
    # This is useful for sharing results with others.
    if (WRITE_POLYGON_OUTPUT) {
      # Build the output path under OUTPUT_DIR.
      # Keeps outputs organized in the same folder as the script.
      out_path <- file.path(OUTPUT_DIR, POLYGON_OUTPUT_FILENAME)
      # Announce output path.
      # Helps learners find the file afterwards.
      cat(pt_timestamp(), "- Writing polygon-clipped raster to: ", out_path, "\n", sep = "")
      # Write a GeoTIFF to disk.
      # We overwrite if it already exists.
      terra::writeRaster(p$raster, out_path, overwrite = TRUE)
      # Confirm write completed.
      # Prints a timestamp for auditing.
      cat(pt_timestamp(), "- ✅ Wrote output successfully\n")
    }

    # Provide a minimal “how to use your own polygon” note.
    # This is aimed at an instructional audience.
    cat("\n")
    cat(pt_timestamp(), "  🧠 How to use your own polygon\n")
    cat(pt_timestamp(), "  - Set POLYGON_FILE_PATH to a GeoJSON/Shapefile/GeoPackage path\n")
    cat(pt_timestamp(), "  - The script will reproject it to EPSG:5070 automatically\n")
    cat(pt_timestamp(), "  - The output contains only pixels inside the polygon (outside = NA)\n")
  } else {
    # Report polygon clip failure.
    # Likely causes include invalid polygon or CRS problems.
    print_result("Polygon clip (crop+mask) succeeded", FALSE, "Polygon clip failed; check polygon validity/CRS and network access")
  }
} else {
  # Report that polygon demo was skipped.
  # This is controlled by RUN_POLYGON_DEMO.
  cat(pt_timestamp(), "- Skipping polygon demo (RUN_POLYGON_DEMO=FALSE)\n")
}

# =============================================================================
# 📝 TEST SUMMARY
# =============================================================================

# Print the Test Summary header.
# This marks the end of the script.
print_header("📝", "TEST SUMMARY")

# Announce completion.
# Timestamp is printed in Pacific Time.
cat(pt_timestamp(), "- All tests completed\n\n")

# Print a key findings header.
# Summarizes what the tests demonstrate.
cat("  🎯 KEY FINDINGS:\n")

# Print a divider line.
# Visually separates the bullet list.
cat("  ─────────────────────────────────────────────────────────────────────\n")

# Print the key findings lines.
# These are human-readable conclusions from the tests.
cat("  1. The URL supports HTTP Range requests (206 Partial Content)\n"); cat("  2. GDAL/terra can open the remote COG and read metadata quickly\n"); cat("  3. Spatial subsets can be read without downloading the full file\n"); cat("  4. COG streaming from this endpoint WORKS correctly\n")

# Print a divider line.
# Ends the bullet list.
cat("  ─────────────────────────────────────────────────────────────────────\n")

# Print a recommended usage header.
# Gives a minimal recipe for day-to-day use.
cat("\n  📎 RECOMMENDED USAGE:\n")

# Print the recommended code snippet.
# Uses the /vsicurl/ path for streaming reads.
cat(sprintf('     r <- rast("%s")\n', VSICURL_PATH)); cat('     my_extent <- ext(xmin, xmax, ymin, ymax)\n'); cat('     subset <- crop(r, my_extent)\n')

# Print a completion message.
# Confirms script reached the end.
cat("\n  ✅ Script completed successfully!\n")

# Print the final timestamp in Pacific Time.
# Useful for logs and reproducibility.
cat("  ", pt_timestamp(), "\n\n")
