# =============================================================================
# extent_checks.R - Check if user extents overlap with WRI data
#
# Purpose:
#   Validate that user-provided bounding boxes or shapefiles overlap with
#   WRI data extent BEFORE attempting to download data. This prevents
#   confusing errors and wasted time downloading empty results.
#
# Key concepts:
#   - Overlap types: none, partial, full
#   - Overlap percentage: how much of user's area is covered by WRI data
#   - Intersection: the region where both extents overlap
#
# =============================================================================

#' Check if user extent overlaps with WRI data extent
#'
#' Determines whether a user-provided extent (bounding box or shapefile bbox)
#' overlaps with the WRI data extent, and if so, by how much. This prevents
#' attempting to download data for regions with no coverage.
#'
#' @param user_extent terra::SpatExtent object in EPSG:5070 (Albers meters).
#'   Typically from \code{validate_bbox()$bbox_5070} or
#'   \code{sf::st_bbox(shapefile) |> terra::ext()}
#'
#' @param wri_extent terra::SpatExtent object in EPSG:5070. Defaults to
#'   \code{fedex::wri_extent$epsg_5070} (the package data object).
#'
#' @return List with 5 elements:
#'   \describe{
#'     \item{overlaps}{Logical. TRUE if any overlap, FALSE if none.}
#'     \item{overlap_type}{Character. One of "none", "partial", "full".}
#'     \item{overlap_pct}{Numeric. Percentage of user extent covered (0-100).}
#'     \item{intersection}{terra::SpatExtent. The overlapping region (or NULL
#'       if no overlap).}
#'     \item{message}{Character. Human-readable description of overlap status.}
#'   }
#'
#' @details
#' **Overlap types:**
#' \itemize{
#'   \item \strong{none} (0%): No overlap. Data download will fail.
#'   \item \strong{partial} (<99.9%): Some overlap. User will get partial data.
#'   \item \strong{full} (≥99.9%): Complete overlap. User gets full requested area.
#' }
#'
#' **Why check overlap?**
#' \itemize{
#'   \item Prevents confusing errors from terra::crop()
#'   \item Gives users informative messages BEFORE downloading
#'   \item Helps users adjust their bounding box if needed
#' }
#'
#' **Overlap percentage calculation:**
#' Calculates what percentage of the user's requested area is covered by
#' WRI data. If user requests 1000 km² but only 500 km² is covered, that's 50%.
#'
#' @examples
#' \dontrun{
#' # Check a bbox in San Francisco (should fully overlap)
#' bbox_sf <- validate_bbox(c(-122, 37, -121, 38))
#' overlap <- check_extent_overlap(bbox_sf$bbox_5070)
#' overlap$overlap_type  # "full"
#' overlap$overlap_pct   # ~100
#'
#' # Check a bbox in Alaska (may not overlap or partially overlap)
#' bbox_ak <- validate_bbox(c(-150, 60, -149, 61))
#' overlap <- check_extent_overlap(bbox_ak$bbox_5070)
#' overlap$overlap_type  # Could be "none" or "partial"
#'
#' # Check a bbox completely outside WRI coverage
#' bbox_europe <- validate_bbox(c(0, 45, 10, 55))
#' overlap <- check_extent_overlap(bbox_europe$bbox_5070)
#' overlap$overlaps      # FALSE
#' overlap$overlap_type  # "none"
#' }
#'
#' @export
check_extent_overlap <- function(user_extent,
                                  wri_extent = fedex::wri_extent$epsg_5070) {

  # --- Step 1: Validate inputs ---
  # Both extents must be terra::SpatExtent objects or numeric vectors
  if (!inherits(user_extent, "SpatExtent")) {
    stop(
      "user_extent must be a terra::SpatExtent object\n",
      "  Typically from: validate_bbox()$bbox_5070\n",
      "  You provided: ", class(user_extent)[1]
    )
  }

  # Convert wri_extent to SpatExtent if it's a numeric vector (from package data)
  if (is.numeric(wri_extent)) {
    wri_extent <- terra::ext(wri_extent)
  } else if (!inherits(wri_extent, "SpatExtent")) {
    stop(
      "wri_extent must be a terra::SpatExtent object or numeric vector\n",
      "  Default is: fedex::wri_extent$epsg_5070\n",
      "  You provided: ", class(wri_extent)[1]
    )
  }

  # --- Step 2: Check for intersection ---
  # terra::intersect() returns the overlapping region, or NULL if no overlap
  intersection <- terra::intersect(user_extent, wri_extent)

  # --- Step 3: Handle no overlap ---
  if (is.null(intersection)) {
    return(list(
      overlaps = FALSE,
      overlap_type = "none",
      overlap_pct = 0,
      intersection = NULL,
      message = "No overlap with WRI data extent"
    ))
  }

  # --- Step 4: Calculate overlap percentage ---
  # How much of the user's requested area is covered by WRI data?

  # Calculate user extent area (in square meters, since EPSG:5070 is in meters)
  user_width <- user_extent$xmax - user_extent$xmin
  user_height <- user_extent$ymax - user_extent$ymin
  user_area <- user_width * user_height

  # Calculate intersection area
  intersection_width <- intersection$xmax - intersection$xmin
  intersection_height <- intersection$ymax - intersection$ymin
  intersection_area <- intersection_width * intersection_height

  # Percentage: (intersection area / user area) * 100
  overlap_pct <- (intersection_area / user_area) * 100

  # --- Step 5: Determine overlap type ---
  # Use 99.9% threshold to account for floating point precision
  if (overlap_pct >= 99.9) {
    overlap_type <- "full"
  } else if (overlap_pct > 0) {
    overlap_type <- "partial"
  } else {
    overlap_type <- "none"
  }

  # --- Step 6: Create human-readable message ---
  if (overlap_type == "full") {
    message_text <- "Requested area fully within WRI data extent"
  } else if (overlap_type == "partial") {
    message_text <- sprintf(
      "%.1f%% of requested area overlaps with WRI data",
      overlap_pct
    )
  } else {
    message_text <- "No overlap with WRI data extent"
  }

  # --- Step 7: Return overlap information ---
  list(
    overlaps = TRUE,
    overlap_type = overlap_type,
    overlap_pct = overlap_pct,
    intersection = intersection,
    message = message_text
  )
}


#' Format extent for human-readable error messages
#'
#' Converts a terra::SpatExtent to a nicely formatted string for error messages.
#' Useful when telling users what the valid extent range is.
#'
#' @param extent terra::SpatExtent object
#' @param crs_label Character. Label for the CRS (e.g., "EPSG:4326", "WGS84")
#' @param decimals Integer. Number of decimal places (default 2)
#'
#' @return Character string in format: "xmin, ymin, xmax, ymax (CRS)"
#'
#' @examples
#' \dontrun{
#' # Format WRI extent in lat/lon
#' format_extent(wri_extent$epsg_4326, "WGS84")
#' # Returns: "-170.00, 32.00, -50.00, 72.00 (WGS84)"
#'
#' # Format in native CRS (meters)
#' format_extent(wri_extent$epsg_5070, "EPSG:5070", decimals = 0)
#' # Returns: "-5216640, 991232, -504690, 6199082 (EPSG:5070)"
#' }
#'
#' @keywords internal
format_extent <- function(extent, crs_label = "", decimals = 2) {
  fmt <- paste0("%.", decimals, "f")

  coords <- sprintf(
    paste(fmt, fmt, fmt, fmt, sep = ", "),
    extent$xmin, extent$ymin, extent$xmax, extent$ymax
  )

  if (nchar(crs_label) > 0) {
    paste0(coords, " (", crs_label, ")")
  } else {
    coords
  }
}