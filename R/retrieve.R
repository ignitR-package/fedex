# =============================================================================
# retrieve.R - Main data retrieval functions
#
# Purpose:
#   Download and spatially subset WRI COG layers from KNB. Handles bounding
#   boxes, shapefiles, CRS transformations, and edge cases gracefully.
#
# Key concepts:
#   - COG (Cloud Optimized GeoTIFF): Raster format supporting partial reads
#   - /vsicurl/: GDAL driver for streaming remote files without full download
#   - crop(): Extract a rectangular region from raster
#   - mask(): Set pixels outside a polygon to NA
#
# Architecture:
#   get_layer() -> validate inputs -> check overlap -> fetch COG -> crop/mask
#
# Reference:
#   R Packages (2e), Chapter 6: R code
#   https://r-pkgs.org/code.html
#
# =============================================================================

#' Get a WRI layer with optional spatial subsetting
#'
#' Retrieves a Wildfire Resilience Index (WRI) COG layer from KNB, with
#' optional cropping to a bounding box or shapefile. Handles CRS
#' transformations, extent validation, and efficient partial reads via
#' GDAL's /vsicurl/ driver.
#'
#' @param layer_id Character. Layer identifier from \code{list_layers()} or
#'   \code{search_layers()}. Currently only "WRI_score" is available (testing).
#'
#' @param bbox Numeric vector c(xmin, ymin, xmax, ymax) in any CRS. If NULL,
#'   no bounding box cropping (default NULL). Auto-detects CRS if not specified.
#'
#' @param shapefile sf object, file path, or terra::SpatVector. If provided,
#'   data will be cropped to shapefile extent and masked to shapefile boundary
#'   (pixels outside polygon set to NA). Default NULL.
#'
#' @param crs CRS of bbox (if NULL, auto-detect). Ignored if shapefile is
#'   provided (shapefile CRS is used). Can be EPSG code, proj4 string, or
#'   sf::st_crs() object. Default NULL.
#'
#' @param simplify Logical. Auto-simplify complex geometries to improve
#'   performance? Default TRUE. Set to FALSE to disable (not recommended for
#'   complex shapefiles with >10,000 vertices).
#'
#' @param simplify_tolerance Numeric. Simplification tolerance in meters
#'   (EPSG:5070). Default 100m, which is comparable to WRI resolution (90m).
#'   Larger values = more aggressive simplification = fewer vertices.
#'
#' @param return_type Character. Output format: "terra" (default) returns
#'   terra::SpatRaster, "stars" returns stars object. Note: stars requires
#'   the stars package to be installed.
#'
#' @param verbose Logical. Print progress messages? Default TRUE. Useful for
#'   understanding what the function is doing, especially for slow operations.
#'
#' @return terra::SpatRaster (or stars object if return_type="stars") containing
#'   the requested WRI layer, cropped/masked to the specified extent.
#'
#' @details
#' **Spatial subsetting:**
#' \enumerate{
#'   \item If \code{bbox} provided: Crop to bounding box rectangle
#'   \item If \code{shapefile} provided: Crop to shapefile extent, then mask
#'     to shapefile boundary (pixels outside polygon = NA)
#'   \item If neither: Return full extent (WARNING: 3.5 GB download)
#' }
#'
#' **CRS handling:**
#' All inputs are automatically transformed to EPSG:5070 (native CRS for
#' WRI COGs). You can provide bbox/shapefile in any CRS:
#' \itemize{
#'   \item EPSG:4326 (WGS84 lat/lon) - most common
#'   \item EPSG:5070 (Albers Equal Area) - native WRI CRS
#'   \item Any other valid CRS
#' }
#'
#' **Efficient streaming:**
#' Uses GDAL's /vsicurl/ driver to stream COG from KNB without downloading
#' the entire file. Only the requested tiles are fetched:
#' \itemize{
#'   \item Full extent: ~3.5 GB
#'   \item Small bbox (50km × 50km): ~15-20 MB
#'   \item Large bbox (200km × 200km): ~200-300 MB
#' }
#'
#' **Performance:**
#' \itemize{
#'   \item Bbox crop: 5-30 seconds (depends on size)
#'   \item Shapefile crop + mask: 10-60 seconds (depends on complexity)
#'   \item Complex shapefiles (>10k vertices): auto-simplified if simplify=TRUE
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Get full extent (WARNING: 3.5 GB)
#' wri_score <- get_layer("WRI_score")
#'
#' # Example 2: Get with bounding box (auto-detects WGS84)
#' wri_sf <- get_layer(
#'   "WRI_score",
#'   bbox = c(-122, 37, -121, 38)
#' )
#'
#' # Example 3: Get with bounding box in EPSG:5070 (meters)
#' wri_albers <- get_layer(
#'   "WRI_score",
#'   bbox = c(-2000000, 1500000, -1950000, 1550000),
#'   crs = 5070
#' )
#'
#' # Example 4: Get with shapefile
#' my_area <- sf::st_read("my_study_area.shp")
#' wri_aoi <- get_layer("WRI_score", shapefile = my_area)
#'
#' # Example 5: Disable simplification (not recommended)
#' complex_area <- sf::st_read("complex_boundary.shp")
#' wri_complex <- get_layer(
#'   "WRI_score",
#'   shapefile = complex_area,
#'   simplify = FALSE
#' )
#'
#' # Example 6: Return as stars object
#' wri_stars <- get_layer(
#'   "WRI_score",
#'   bbox = c(-122, 37, -121, 38),
#'   return_type = "stars"
#' )
#' }
#'
#' @export
get_layer <- function(layer_id,
                      bbox = NULL,
                      shapefile = NULL,
                      crs = NULL,
                      simplify = TRUE,
                      simplify_tolerance = 100,
                      return_type = c("terra", "stars"),
                      verbose = TRUE) {

  # --- Step 1: Validate inputs ---
  return_type <- match.arg(return_type)

  if (!is.null(bbox) && !is.null(shapefile)) {
    warning("Both bbox and shapefile provided. Shapefile takes precedence (bbox ignored).")
  }

  # --- Step 2: Get COG URL from STAC catalog ---
  if (verbose) message("Querying STAC catalog for layer: ", layer_id)

  # Load all STAC items from package
  items <- load_stac_items()

  # Find matching item by ID
  matching_item <- Filter(function(x) x$id == layer_id, items)

  if (length(matching_item) == 0) {
    # Layer not found in STAC catalog
    # Show available layers to help user
    all_ids <- sapply(items, function(x) x$id)

    stop(
      "Layer not found in STAC catalog: '", layer_id, "'\n\n",
      "Available layers (", length(all_ids), " total):\n",
      paste("  -", head(all_ids, 10), collapse = "\n"),
      if (length(all_ids) > 10) paste0("\n  ... and ", length(all_ids) - 10, " more"),
      "\n\nUse load_stac_items() to see all available layers and their metadata.\n",
      "Use list_stac_properties(load_stac_items()) to see filterable properties."
    )
  }

  item <- matching_item[[1]]

  # Extract COG URL from item asset
  cog_url <- get_asset_urls(item, asset_type = "data")

  if (is.null(cog_url) || length(cog_url) == 0) {
    stop(
      "Layer found in STAC catalog but no data asset URL available: '", layer_id, "'\n\n",
      "This may indicate a problem with the STAC catalog generation.\n",
      "Item properties:\n",
      "  - Domain: ", item$properties$wri_domain, "\n",
      "  - Data type: ", item$properties$data_type, "\n",
      "  - Layer type: ", item$properties$wri_layer_type
    )
  }

  # Check if URL points to a hosted file (KNB) or local file
  is_hosted <- grepl("^https?://", cog_url)

  if (!is_hosted) {
    # URL is a local file path (relative to STAC root)
    # This happens when the file hasn't been uploaded to KNB yet

    domain <- if (!is.null(item$properties$wri_domain)) item$properties$wri_domain else "unknown"
    dtype <- if (!is.null(item$properties$data_type)) item$properties$data_type else "unknown"
    ltype <- if (!is.null(item$properties$wri_layer_type)) item$properties$wri_layer_type else "unknown"

    stop(
      "Layer '", layer_id, "' is not available for remote access.\n\n",
      "This layer exists in the STAC catalog but has not been uploaded to KNB yet.\n",
      "Local COG path: ", cog_url, "\n\n",
      "Layer metadata:\n",
      "  - Domain: ", domain, "\n",
      "  - Data type: ", dtype, "\n",
      "  - Layer type: ", ltype, "\n\n",
      "During the testing phase, only certain layers are hosted on KNB.\n",
      "If you need this layer, contact the package maintainer about uploading it to KNB."
    )
  }

  if (verbose) {
    message(
      "Found layer in catalog:\n",
      "  - ID: ", item$id, "\n",
      "  - Domain: ", item$properties$wri_domain, "\n",
      "  - Data type: ", item$properties$data_type, "\n",
      "  - URL: ", cog_url
    )
  }

  # --- Step 3: Setup GDAL for efficient remote access ---
  # These settings reduce unnecessary HTTP requests and improve performance
  Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")  # Don't list directory
  Sys.setenv(CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif")   # Only allow .tif

  # Construct /vsicurl/ path for GDAL to stream the COG
  vsicurl_path <- paste0("/vsicurl/", cog_url)

  # --- HANDLE SPATIAL SUBSETTING ---

  # ====== CASE 1: Shapefile provided ======
  if (!is.null(shapefile)) {

    if (verbose) message("Validating shapefile...")

    # Validate and transform to EPSG:5070
    shapefile <- validate_shapefile(shapefile, crs)

    # Check complexity and simplify if needed
    if (simplify) {
      shapefile <- simplify_if_needed(
        shapefile,
        max_vertices = 10000,
        tolerance = simplify_tolerance,
        verbose = verbose
      )
    }

    # Get bounding box from shapefile for initial crop
    bbox_5070 <- sf::st_bbox(shapefile) |> terra::ext()

    # Check if shapefile overlaps with WRI data
    # wri_extent is numeric vector, gets converted to SpatExtent in check_extent_overlap()
    overlap <- check_extent_overlap(bbox_5070, wri_extent$epsg_5070)

    if (!overlap$overlaps) {
      # Convert wri_extent vector to terra::ext() for format_extent()
      wri_ext_4326 <- terra::ext(wri_extent$epsg_4326)
      stop(
        "Shapefile does not overlap with WRI data extent.\n\n",
        "WRI extent (EPSG:4326): ",
        format_extent(wri_ext_4326, "WGS84"), "\n",
        "Your shapefile extent (EPSG:4326): ",
        format_extent(
          sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(shapefile)), 4326)) |> terra::ext(),
          "WGS84"
        )
      )
    }

    if (overlap$overlap_type == "partial" && verbose) {
      message(overlap$message, "\nReturning data for overlapping region only.")
    }

    # Fetch and process
    if (verbose) message("Fetching data from KNB...")
    r <- terra::rast(vsicurl_path)

    if (verbose) message("Cropping to shapefile extent...")
    r_cropped <- terra::crop(r, bbox_5070, snap = "out")

    if (verbose) message("Masking to shapefile boundary...")
    r_masked <- terra::mask(r_cropped, terra::vect(shapefile))

    result <- r_masked

  # ====== CASE 2: Bounding box provided ======
  } else if (!is.null(bbox)) {

    if (verbose) message("Validating bounding box...")

    # Validate and transform to EPSG:5070
    bbox_valid <- validate_bbox(bbox, crs)

    # Check if bbox overlaps with WRI data
    # wri_extent is numeric vector, gets converted to SpatExtent in check_extent_overlap()
    overlap <- check_extent_overlap(bbox_valid$bbox_5070, wri_extent$epsg_5070)

    if (!overlap$overlaps) {
      # Convert wri_extent vector to terra::ext() for format_extent()
      wri_ext_4326 <- terra::ext(wri_extent$epsg_4326)
      stop(
        "Bounding box does not overlap with WRI data extent.\n\n",
        "WRI extent (EPSG:4326): ",
        format_extent(wri_ext_4326, "WGS84"), "\n",
        "Your bbox (EPSG:4326): ",
        format_extent(bbox_valid$bbox_4326, "WGS84")
      )
    }

    if (overlap$overlap_type == "partial" && verbose) {
      message(overlap$message, "\nReturning data for overlapping region only.")
    }

    # Fetch and crop
    if (verbose) message("Fetching data from KNB...")
    r <- terra::rast(vsicurl_path)

    if (verbose) message("Cropping to bounding box...")
    result <- terra::crop(r, bbox_valid$bbox_5070, snap = "out")

  # ====== CASE 3: Full extent (no subsetting) ======
  } else {

    if (verbose) {
      message(
        "No spatial subset specified. Fetching full extent...\n",
        "WARNING: This will download ~3.5 GB. Consider using bbox= or shapefile=\n",
        "Press Ctrl+C to cancel or wait for download to begin."
      )
      Sys.sleep(2)  # Give user time to cancel
    }

    # Fetch full extent
    result <- terra::rast(vsicurl_path)

    # Force full read (actually download all data)
    if (verbose) message("Reading full raster (this may take several minutes)...")
    result <- terra::readAll(result)
  }

  # --- Step 4: Convert to stars if requested ---
  if (return_type == "stars") {
    if (!requireNamespace("stars", quietly = TRUE)) {
      stop(
        "Package 'stars' required for return_type='stars'.\n",
        "Install with: install.packages('stars')"
      )
    }
    result <- stars::st_as_stars(result)
  }

  # --- Step 5: Done! ---
  if (verbose) message("Complete!")

  result
}
