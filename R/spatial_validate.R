# =============================================================================
# spatial_validate.R - Validate and standardize spatial inputs
#
# Purpose:
#   Validate bounding boxes and shapefiles provided by users, transforming
#   them to the native CRS (EPSG:5070) needed for cropping WRI COG layers.
#
# Key concepts:
#   - CRS (Coordinate Reference System): How coordinates map to Earth locations
#   - EPSG:4326 = WGS84 (lat/lon), EPSG:5070 = Albers Equal Area (meters)
#   - Transform: Converting coordinates from one CRS to another
#
# Reference:
#   R Packages (2e), Chapter 6: R code
#   https://r-pkgs.org/code.html
#
# =============================================================================

#' Validate and standardize bounding box input
#'
#' Takes a user-provided bounding box (in any CRS) and validates it, then
#' transforms it to both EPSG:5070 (for data retrieval) and EPSG:4326
#' (for error messages). Auto-detects CRS if not provided.
#'
#' @param bbox Numeric vector c(xmin, ymin, xmax, ymax). Can be in any CRS.
#' @param crs CRS of the bbox. Can be:
#'   \itemize{
#'     \item NULL (default) - auto-detect based on coordinate values
#'     \item Integer EPSG code (e.g., 4326 or 5070)
#'     \item Proj4 string (e.g., "+proj=longlat +datum=WGS84")
#'     \item sf::st_crs() object
#'   }
#'
#' @return List with three elements:
#'   \describe{
#'     \item{bbox_5070}{terra::SpatExtent in EPSG:5070 (for data retrieval)}
#'     \item{bbox_4326}{terra::SpatExtent in EPSG:4326 (for messages)}
#'     \item{original_crs}{The CRS that was detected or provided}
#'   }
#'
#' @details
#' **Auto-detection logic:**
#' \itemize{
#'   \item If all x-values are in [-180, 180] and y-values in [-90, 90],
#'     assumes EPSG:4326 (lat/lon)
#'   \item If all values are > 180, assumes EPSG:5070 (Albers, meters)
#'   \item Otherwise, throws error asking user to specify CRS
#' }
#'
#' **Why transform to both CRS?**
#' \itemize{
#'   \item EPSG:5070 is needed for cropping (native CRS of WRI COGs)
#'   \item EPSG:4326 is needed for human-readable error messages
#' }
#'
#' @examples
#' \dontrun{
#' # Auto-detect WGS84 (lat/lon)
#' bbox_sf <- validate_bbox(c(-122, 37, -121, 38))
#' # Message: "Auto-detected bbox CRS as EPSG:4326..."
#'
#' # Explicit EPSG:5070 (meters)
#' bbox_albers <- validate_bbox(
#'   c(-2000000, 1500000, -1950000, 1550000),
#'   crs = 5070
#' )
#'
#' # Access the transformed extents
#' bbox_sf$bbox_5070  # For cropping COG
#' bbox_sf$bbox_4326  # For error messages
#' }
#'
#' @export
validate_bbox <- function(bbox, crs = NULL) {

  # --- Step 1: Validate format ---
  # Bbox must be a numeric vector with exactly 4 elements
  if (!is.numeric(bbox) || length(bbox) != 4) {
    stop(
      "bbox must be a numeric vector with 4 elements: c(xmin, ymin, xmax, ymax)\n",
      "  Example: c(-122, 37, -121, 38)  # WGS84 lat/lon\n",
      "  Example: c(-2000000, 1500000, -1950000, 1550000)  # EPSG:5070 meters"
    )
  }

  # --- Step 2: Validate order ---
  # xmin must be less than xmax, ymin must be less than ymax
  if (bbox[1] >= bbox[3]) {
    stop(
      "Invalid bbox: xmin (", bbox[1], ") must be < xmax (", bbox[3], ")\n",
      "  You provided: c(", paste(bbox, collapse = ", "), ")"
    )
  }

  if (bbox[2] >= bbox[4]) {
    stop(
      "Invalid bbox: ymin (", bbox[2], ") must be < ymax (", bbox[4], ")\n",
      "  You provided: c(", paste(bbox, collapse = ", "), ")"
    )
  }

  # --- Step 3: Determine CRS (auto-detect if not provided) ---
  if (is.null(crs)) {
    crs <- detect_bbox_crs(bbox)
  }

  # --- Step 4: Convert to sf bbox object ---
  # sf::st_bbox creates a bbox with associated CRS
  # This enables transformation to other CRS
  bbox_sf <- sf::st_bbox(
    c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]),
    crs = sf::st_crs(crs)  # Attach the CRS
  )

  # --- Step 5: Transform to EPSG:5070 (native CRS for data retrieval) ---
  # WRI COGs are in EPSG:5070, so we need bbox in that CRS for cropping
  # Steps:
  #   1. Convert bbox to sfc (spatial geometry)
  #   2. Transform to EPSG:5070
  #   3. Extract bbox from transformed geometry
  #   4. Convert to terra::ext (terra's extent format)
  bbox_5070 <- bbox_sf |>
    sf::st_as_sfc() |>
    sf::st_transform(5070) |>
    sf::st_bbox() |>
    terra::ext()

  # --- Step 6: Transform to EPSG:4326 (for human-readable messages) ---
  # Used in error messages so users can see lat/lon coordinates
  bbox_4326 <- bbox_sf |>
    sf::st_as_sfc() |>
    sf::st_transform(4326) |>
    sf::st_bbox() |>
    terra::ext()

  # --- Step 7: Return all three versions ---
  list(
    bbox_5070 = bbox_5070,      # For data retrieval
    bbox_4326 = bbox_4326,      # For messages
    original_crs = crs          # For reference
  )
}


#' Auto-detect CRS from bounding box values
#'
#' Uses heuristics to guess the CRS based on coordinate magnitudes.
#' This is convenient but can fail for ambiguous cases.
#'
#' @param bbox Numeric vector c(xmin, ymin, xmax, ymax)
#'
#' @return Integer EPSG code (4326 or 5070)
#'
#' @details
#' **Detection logic:**
#' \itemize{
#'   \item If x in [-180, 180] and y in [-90, 90]: EPSG:4326 (lat/lon)
#'   \item If all values > 180: EPSG:5070 (Albers, meters)
#'   \item Otherwise: error (ambiguous)
#' }
#'
#' @keywords internal
detect_bbox_crs <- function(bbox) {

  # --- Check for WGS84 (EPSG:4326) ---
  # Longitude is in [-180, 180], latitude is in [-90, 90]
  x_looks_like_lon <- all(abs(bbox[c(1, 3)]) <= 180)
  y_looks_like_lat <- all(abs(bbox[c(2, 4)]) <= 90)

  if (x_looks_like_lon && y_looks_like_lat) {
    message("Auto-detected bbox CRS as EPSG:4326 (WGS84 lat/lon). Specify crs= to override.")
    return(4326)
  }

  # --- Check for EPSG:5070 (Albers Equal Area) ---
  # Albers uses meters, so values are typically in millions
  # All values should be > 180 (to distinguish from lat/lon)
  if (all(abs(bbox) > 180)) {
    message("Auto-detected bbox CRS as EPSG:5070 (Albers Equal Area, meters). Specify crs= to override.")
    return(5070)
  }

  # --- Cannot detect ---
  # Values are ambiguous (e.g., small positive numbers)
  stop(
    "Cannot auto-detect CRS from bbox values. Please specify crs= parameter.\n",
    "  Your bbox: c(", paste(bbox, collapse = ", "), ")\n",
    "  Common options:\n",
    "    crs = 4326  # WGS84 (lat/lon)\n",
    "    crs = 5070  # Albers Equal Area (meters)\n\n",
    "  Example:\n",
    "    validate_bbox(c(-122, 37, -121, 38), crs = 4326)"
  )
}


#' Validate shapefile input
#'
#' Takes a user-provided shapefile (sf object, file path, or SpatVector)
#' and validates it, then transforms it to EPSG:5070 (native CRS for WRI COGs).
#'
#' @param shapefile One of:
#'   \itemize{
#'     \item sf object (from sf::st_read() or sf::st_as_sf())
#'     \item Character file path to shapefile (.shp, .gpkg, .geojson, etc.)
#'     \item terra::SpatVector object
#'   }
#' @param crs Expected CRS (if NULL, use the shapefile's CRS). Rarely needed.
#'
#' @return sf object in EPSG:5070 (ready for cropping/masking)
#'
#' @details
#' **Processing steps:**
#' \enumerate{
#'   \item Convert to sf if needed (handles file paths and SpatVector)
#'   \item Check for invalid geometries and fix if needed
#'   \item Transform to EPSG:5070 if not already in that CRS
#'   \item Return clean sf object
#' }
#'
#' **Geometry validation:**
#' Invalid geometries (self-intersections, holes, etc.) can cause errors
#' when cropping/masking. This function uses sf::st_make_valid() to fix
#' common issues automatically.
#'
#' @examples
#' \dontrun{
#' # From file path
#' my_area <- validate_shapefile("data/study_area.shp")
#'
#' # From sf object
#' my_area <- sf::st_read("data/study_area.shp")
#' my_area_5070 <- validate_shapefile(my_area)
#'
#' # Check the result
#' sf::st_crs(my_area_5070)$epsg  # Should be 5070
#' }
#'
#' @export
validate_shapefile <- function(shapefile, crs = NULL) {

  # --- Step 1: Convert to sf object if needed ---

  # Case 1: File path (character string)
  if (inherits(shapefile, "character")) {
    if (!file.exists(shapefile)) {
      stop("Shapefile not found: ", shapefile)
    }
    shapefile <- sf::st_read(shapefile, quiet = TRUE)
  }

  # Case 2: terra::SpatVector
  # Convert to sf for consistency
  else if (inherits(shapefile, "SpatVector")) {
    shapefile <- sf::st_as_sf(shapefile)
  }

  # Case 3: Should be sf object at this point
  else if (!inherits(shapefile, "sf")) {
    stop(
      "shapefile must be one of:\n",
      "  - sf object (from sf::st_read())\n",
      "  - File path to shapefile (.shp, .gpkg, .geojson, etc.)\n",
      "  - terra::SpatVector object\n\n",
      "  You provided: ", class(shapefile)[1]
    )
  }

  # --- Step 2: Validate geometries ---
  # Invalid geometries can cause terra::crop() and terra::mask() to fail
  # Common issues: self-intersections, unclosed rings, etc.
  invalid_geoms <- !sf::st_is_valid(shapefile)

  if (any(invalid_geoms)) {
    n_invalid <- sum(invalid_geoms)
    warning(
      "Shapefile has ", n_invalid, " invalid geometr",
      ifelse(n_invalid == 1, "y", "ies"), ". Attempting to fix with st_make_valid()..."
    )
    shapefile <- sf::st_make_valid(shapefile)
  }

  # --- Step 3: Transform to EPSG:5070 if needed ---
  # WRI COGs are in EPSG:5070, so shapefile must match for cropping/masking
  current_crs <- sf::st_crs(shapefile)$epsg

  if (is.na(current_crs)) {
    stop(
      "Shapefile has no CRS defined. Please assign a CRS first:\n",
      "  shapefile <- sf::st_set_crs(shapefile, <EPSG_CODE>)\n\n",
      "  Common options:\n",
      "    4326  # WGS84 (lat/lon)\n",
      "    5070  # Albers Equal Area (meters)"
    )
  }

  if (current_crs != 5070) {
    message("Transforming shapefile from EPSG:", current_crs, " to EPSG:5070 (WRI native CRS)")
    shapefile <- sf::st_transform(shapefile, 5070)
  }

  # --- Step 4: Return validated shapefile ---
  shapefile
}