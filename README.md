# fedex: Access WRI Data from R

<!-- badges: start -->
<!-- badges: end -->

The **fedex** package provides convenient R functions to access Wildfire Resilience Index (WRI) raster data hosted on the Knowledge Network for Biocomplexity (KNB). The package uses Cloud Optimized GeoTIFFs (COGs) to enable efficient spatial subsetting - download only the pixels you need, not the entire 3.5 GB file.

## Key Features

- **Efficient data retrieval** - Stream COG tiles via HTTP range requests
- **Spatial subsetting** - Extract data by bounding box or shapefile
- **Automatic CRS handling** - Accepts lat/lon (EPSG:4326) or Albers (EPSG:5070)
- **Built-in validation** - Checks extent overlap and geometry complexity
- **STAC catalog** - Query available layers and metadata

## Installation

```r
# Install from GitHub (when available)
# remotes::install_github("your-org/fedex")

# For now, install from local source:
devtools::install("/path/to/fedex")
```

## Quick Start

```r
library(fedex)

# Example 1: Get data for a bounding box (San Francisco Bay Area)
# Coordinates in lat/lon (EPSG:4326)
wri_sf <- get_layer(
  layer_id = 'WRI_score',
  bbox = c(-122.5, 37.5, -121.5, 38.0)
)

# Example 2: Get data for a shapefile
library(sf)
my_area <- st_read("my_study_area.shp")
wri_clip <- get_layer(
  layer_id = 'WRI_score',
  shapefile = my_area
)

# Example 3: Get full extent (downloads entire file - use sparingly!)
wri_full <- get_layer('WRI_score')

# Visualize result
plot(wri_sf, main = "WRI Score - SF Bay Area")
```

## How It Works

### Cloud Optimized GeoTIFFs (COGs)

Traditional workflow:
```
Download 3.5 GB → Crop to your area → Use 50 MB
```

COG workflow:
```
Request your area via HTTP → Download 50 MB → Done
```

COGs are internally tiled and include pre-computed overviews (image pyramids), enabling:
- **Partial reads** - Download only the tiles covering your area of interest
- **Fast metadata** - Read dimensions/CRS without downloading pixel data
- **Multi-scale access** - Quickly get low-resolution previews

### STAC Catalog

The package includes a SpatioTemporal Asset Catalog (STAC) with metadata for all available layers:

```r
# List all available layers
items <- load_stac_items()
length(items)  # Number of layers

# Get layer properties
get_domains(items)  # List of domain names
get_asset_urls(items[[1]])  # COG URL for first layer
```

## Function Reference

### Main Functions

- `get_layer()` - Retrieve raster data with optional spatial subsetting
- `load_stac_items()` - Load STAC catalog items
- `get_domains()` - List available domain names from STAC
- `get_asset_urls()` - Extract asset URLs from STAC item

### Spatial Utilities

- `validate_bbox()` - Validate and transform bounding boxes
- `validate_shapefile()` - Validate and reproject shapefiles
- `check_extent_overlap()` - Check if user extent overlaps with WRI data
- `measure_complexity()` - Count vertices in geometry
- `simplify_if_needed()` - Auto-simplify complex geometries

## Examples

### Bounding Box Options

```r
# Auto-detect CRS from coordinates
wri1 <- get_layer('WRI_score', bbox = c(-122, 37, -121, 38))  # lat/lon

# Explicit CRS (EPSG:5070 - Albers Equal Area)
wri2 <- get_layer('WRI_score',
                  bbox = c(-2000000, 1500000, -1900000, 1600000),
                  crs = 5070)
```

### Shapefile Options

```r
# Simple shapefile
my_polygon <- st_read("boundary.shp")
wri <- get_layer('WRI_score', shapefile = my_polygon)

# Auto-simplification for complex geometries
wri_auto <- get_layer('WRI_score',
                      shapefile = very_detailed_boundary,
                      simplify = TRUE,
                      simplify_tolerance = 100)  # meters in EPSG:5070
```

### Return Types

```r
# Default: terra SpatRaster
wri_terra <- get_layer('WRI_score', bbox = my_bbox, return_type = "terra")

# stars object (for tidy workflows)
wri_stars <- get_layer('WRI_score', bbox = my_bbox, return_type = "stars")
```

## Package Data

The package includes pre-computed extent information:

```r
# WRI data extent and properties
fedex::wri_extent
# $epsg_5070: Extent in native Albers projection
# $epsg_4326: Extent in WGS84 lat/lon
# $resolution: 90m × 90m pixels
# $dimensions: 52355 cols × 57865 rows
```

## Performance

Typical download sizes for different spatial extents:

| Extent Size | Data Downloaded | Time (approximate) |
|-------------|-----------------|-------------------|
| 10 km × 10 km | ~0.5 MB | 2-3 seconds |
| 50 km × 50 km | ~10 MB | 5-10 seconds |
| 200 km × 200 km | ~150 MB | 30-60 seconds |
| San Francisco Bay | ~50 MB | 15-20 seconds |
| Full California | ~800 MB | 2-3 minutes |
| Full extent | 3.5 GB | 10-15 minutes |

**Tip:** Always subset to your study area to minimize download time and memory usage.

## Demonstrations

See [`demos/`](demos/) for comprehensive COG streaming verification scripts:
- **test_cog_streaming_verified.R** - Full test suite with 6 validation tests
- **original_test_cog_steaming.R** - Archived detailed version
- **test_overview_access.R** - Comparison of overview access methods

Run the main demo:
```bash
Rscript demos/test_cog_streaming_verified.R
```

## Data Source

Data is hosted on the Knowledge Network for Biocomplexity (KNB):
- **URL:** https://knb.ecoinformatics.org/
- **Layer example:** [WRI_score.tif](https://knb.ecoinformatics.org/data/WRI_score.tif)
- **Access:** Public, no authentication required
- **Format:** Cloud Optimized GeoTIFF with 7 overview levels

## Development

This package follows [R Packages (2e)](https://r-pkgs.org/) best practices.

```r
# Load package during development
devtools::load_all()

# Update documentation
devtools::document()

# Run tests
devtools::test()

# Check package
devtools::check()
```

### Package Structure

```
fedex/
├── R/                    # Package functions
│   ├── retrieve.R       # Main get_layer() function
│   ├── spatial_validate.R  # Bbox and shapefile validation
│   ├── extent_checks.R  # Overlap checking
│   ├── complexity.R     # Geometry simplification
│   ├── stac_utils.R     # STAC catalog utilities
│   └── data.R           # Package data documentation
├── data/                # Package data objects (.rda)
├── data-raw/            # Scripts to generate package data
├── inst/extdata/        # STAC catalog and metadata (ships with package)
├── man/                 # Generated documentation
├── demos/               # Demonstration scripts
└── tests/               # Unit tests
```

## Development

This package follows [R Packages (2e)](https://r-pkgs.org/) best practices.

**For developers and data managers:**
- See [CONTRIBUTING.md](CONTRIBUTING.md) for:
  - Architecture overview (backend vs frontend)
  - How to update STAC when data changes
  - Development workflow and testing
  - Design decisions and references

```r
# Load package during development
devtools::load_all()

# Update documentation
devtools::document()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## Contributing

Please report issues or suggest improvements via GitHub Issues.

For development contributions, see [CONTRIBUTING.md](CONTRIBUTING.md).

## License

MIT License - see [LICENSE.md](LICENSE.md)

## References

- [R Packages (2e)](https://r-pkgs.org/) - R package development guide
- [Cloud Optimized GeoTIFF](https://www.cogeo.org/)
- [STAC Specification](https://stacspec.org/)
- [GDAL Virtual File Systems](https://gdal.org/user/virtual_file_systems.html)
- [terra R package](https://rspatial.github.io/terra/)
