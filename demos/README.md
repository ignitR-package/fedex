# COG Streaming Demonstration Scripts

This directory contains demonstration scripts that test and verify Cloud Optimized GeoTIFF (COG) streaming capabilities for the WRI data hosted on KNB. These scripts are **separate from the package functionality** and serve as examples and validation tests.

---

## Scripts

### [test_cog_streaming_verified.R](test_cog_streaming_verified.R) ✅ **CURRENT DEMO**

**Purpose:** Comprehensive COG streaming verification with 6 tests

**What it tests:**
1. **HTTP Range Requests** - Verifies server supports partial downloads (HTTP 206)
2. **Fast Metadata Access** - Opens COG and reads dimensions/CRS without downloading data
3. **Small Bbox Subset** - Crops to 50km area, reads only needed tiles
4. **Scaling Behavior** - Tests 10km, 50km, 100km, 200km windows to verify proportional reads
5. **Polygon Clip** - Demonstrates crop + mask workflow for irregular boundaries
6. **Overview Access** - Reads pre-computed low-resolution preview efficiently

**Outputs:** 4 PNG visualizations + console test results

**Run time:** ~2-3 minutes (all tests)

**Usage:**
```r
Rscript demos/test_cog_streaming_verified.R
```

**Status:** ✅ All tests passing, uses correct methods

---

### [original_test_cog_steaming.R](original_test_cog_steaming.R) 📦 **ARCHIVE**

**Purpose:** Original comprehensive test script with detailed documentation

**Why archived:**
- Contains overly verbose comments (2 comment lines per code line)
- Test 6 (overview access) uses inefficient method
- Kept for historical reference and detailed COG concepts documentation

**Note:** This script is extremely well-documented and can serve as a teaching resource for understanding COG internals, but [test_cog_streaming_verified.R](test_cog_streaming_verified.R) should be used for actual testing

---

## The Overview Access Issue 🔍

### Background

Cloud Optimized GeoTIFFs include **pre-computed overview levels** (image pyramids) for efficient multi-scale access:

| Level | Reduction | Approx Size | Use Case |
|-------|-----------|-------------|----------|
| Full  | 1×        | 3.5 GB      | Full resolution analysis |
| 1     | 2×        | 900 MB      | Regional analysis |
| 2     | 4×        | 225 MB      | State-level views |
| 3     | 8×        | 56 MB       | Quick previews |
| 4     | 16×       | 14 MB       | Overview maps |
| **5** | **32×**   | **3.5 MB**  | **Fast low-res preview** ← We read this |
| 6     | 64×       | 900 KB      | Thumbnail |
| 7     | 128×      | 225 KB      | Tiny preview |

### The Problem

The original demo script used `terra::aggregate()` for Test 6:

```r
r_overview <- aggregate(r, fact = 32, fun = "mean", na.rm = TRUE)
```

**What this does (WRONG):**
- Downloads the **entire 3.5 GB file** from the server
- Computes averages in R for every 32×32 pixel window
- **Ignores** the pre-computed overview level 5 that already exists in the COG
- Takes ~6 minutes

### The Solution

Use `gdal_translate` with `-outsize` to request specific dimensions - GDAL automatically reads the matching overview level:

```r
temp_overview <- tempfile(fileext = ".tif")

gdal_translate(
  src_dataset = VSICURL_PATH,
  dst_dataset = temp_overview,
  outsize = c(round(ncol(r)/32), round(nrow(r)/32)),
  r = "average"
)

r_overview <- rast(temp_overview)
vals_overview <- values(r_overview)
unlink(temp_overview)
```

**What this does (CORRECT):**
- Tells GDAL: "I want a 1/32 size version"
- GDAL reads overview level 5 (~3-5 MB)
- No computation needed - just reads existing pyramid data
- Takes ~25 seconds

### Performance Comparison

| Method | Time | Data Downloaded | Recommended |
|--------|------|-----------------|-------------|
| `aggregate()` | ~6 min | ~3.5 GB | ❌ NO |
| `gdal_translate -outsize` | ~25 sec | ~3-5 MB | ✅ YES |

**Speedup:** 12× faster

---

## Key Concepts

### Cloud Optimized GeoTIFF (COG)

A regular GeoTIFF with:
1. **Tiled structure** - Data organized in ~256×256 pixel chunks
2. **Internal overviews** - Pre-computed lower-resolution pyramids
3. **HTTP range request support** - Server allows partial file downloads

### Why COGs Matter for Remote Data

**Traditional workflow:**
```
Download 3.5 GB → Crop to your area → Use 50 MB
```

**COG workflow:**
```
Request your area via HTTP ranges → Download 50 MB → Done
```

### GDAL `/vsicurl/` Driver

GDAL's virtual file system for HTTP streaming:
- Prefix any URL with `/vsicurl/` to enable streaming
- GDAL automatically uses HTTP range requests
- Only downloads the tiles you need

**Example:**
```r
Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
Sys.setenv(CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif")

url <- "https://knb.ecoinformatics.org/data/WRI_score.tif"
vsicurl_path <- paste0("/vsicurl/", url)

r <- rast(vsicurl_path)  # Opens metadata only (~few KB)
subset <- crop(r, my_extent)  # Downloads only needed tiles
```

---

## Running the Demos

### Prerequisites

```r
install.packages(c("terra", "sf", "httr", "gdalUtilities"))
```

### Quick Test

```bash
cd /home/shares/wwri-wildfire/MEDS-2026/fedex
Rscript demos/test_cog_streaming_verified.R
```

### Understanding Output

Each test prints:
- **[OK]** or **[FAIL]** status
- Timing information
- Data transfer statistics
- Percentage of full file downloaded

**Good results:**
- Metadata read: < 5 seconds
- Small subset: < 1% of full file
- Scaling: larger areas download more data (proportional)
- Overview access: < 60 seconds

---

## Integration with fedex Package

The `fedex` package uses the same COG streaming principles demonstrated here:

```r
library(fedex)

# Small area (downloads ~10 MB)
wri <- get_layer('WRI_score', bbox = c(-122, 37.5, -121.9, 37.6))

# Shapefile clip (downloads only needed tiles)
wri <- get_layer('WRI_score', shapefile = my_sf_polygon)

# Full extent (downloads full 3.5 GB - use sparingly!)
wri <- get_layer('WRI_score')
```

See [../R/retrieve.R](../R/retrieve.R) for implementation details.

---

## References

- [Cloud Optimized GeoTIFF Specification](https://www.cogeo.org/)
- [GDAL Virtual File Systems](https://gdal.org/user/virtual_file_systems.html)
- [WRI Data on KNB](https://knb.ecoinformatics.org/)

---

## Summary

✅ **All WRI COGs are properly configured** - 7 overview levels, efficient tiling
✅ **COG streaming works perfectly** from KNB
✅ **Use the verified demo** for testing and examples
✅ **Use `gdal_translate`** for overview access, NOT `aggregate()`

The demos prove that users can access WRI data efficiently via HTTP range requests without downloading full files.
