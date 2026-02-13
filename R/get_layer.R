#' Retrieve a WRI layer as raster
#'
#' Download and optionally crop a WRI layer to a bounding box. For hosted layers,
#' this uses COG streaming to fetch only the requested spatial subset.
#'
#' @param id Character. Layer name (without .tif extension). Use `list_layers()`
#'   to see available layers.
#' @param bbox Numeric vector of length 4, optional. Bounding box as c(xmin, ymin, xmax, ymax)
#'   in EPSG:4326 (longitude/latitude). If NULL, returns the full raster.
#'
#' @returns A terra SpatRaster object, optionally cropped to the bounding box
#' @export
#'
#' @examples
#' # Get the full WRI score layer
#' wri_full <- get_layer("WRI_score")
#' terra::plot(wri_full)
#'
#' # Get WRI score for northern California
#' bbox_norcal <- c(-122, 37, -121, 38)
#' wri_norcal <- get_layer("WRI_score", bbox = bbox_norcal)
#' terra::plot(wri_norcal)
#'
#' # Get an air quality resistance indicator
#' asthma <- get_layer("air_quality_resistance_asthma", bbox = bbox_norcal)
#' terra::plot(asthma)
get_layer <- function(id, bbox = NULL) {

  # Validate inputs
  if (missing(id) || is.null(id) || id == "") {
    stop("Layer 'id' is required. Use list_layers() to see available layers.")
  }

  if (!is.null(bbox) && length(bbox) != 4) {
    stop("'bbox' must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax)")
  }

  # Get all layers and find the matching one
  all_layers <- list_layers()

  if (!id %in% all_layers$id) {
    stop(
      "Layer '", id, "' not found in STAC catalog.\n",
      "Available layers: ", paste(head(all_layers$id, 10), collapse = ", "),
      if (nrow(all_layers) > 10) "..." else ""
    )
  }

  # Get the specific layer info
  layer_info <- all_layers[all_layers$id == id, ]

  # Check if layer is hosted
  if (!layer_info$is_hosted) {
    stop(
      "Layer '", id, "' is not yet hosted on KNB.\n",
      "This layer is only available locally for development.\n",
      "See list_layers(hosted_only = TRUE) for available layers."
    )
  }

  # Get the href (should be a KNB URL)
  href <- layer_info$href

  # Build GDAL virtual file path for COG streaming
  vsi_path <- paste0("/vsicurl/", href)

  # Read the raster and optionally crop to bbox
  # This uses HTTP range requests to stream only needed tiles
  tryCatch({
    # Load the raster
    rast <- terra::rast(vsi_path)

    # If bbox provided, crop to it
    if (!is.null(bbox)) {
      # Create bbox extent in EPSG:4326 (lon/lat)
      bbox_ext <- terra::ext(bbox[1], bbox[3], bbox[2], bbox[4])
      bbox_vect <- terra::as.polygons(bbox_ext, crs = "EPSG:4326")

      # Transform bbox to match raster's CRS
      bbox_transformed <- terra::project(bbox_vect, terra::crs(rast))

      # Crop using transformed bbox
      rast <- terra::crop(rast, bbox_transformed)
    }

    rast
  }, error = function(e) {
    stop(
      "Failed to retrieve layer '", id, "'.\n",
      "URL: ", href, "\n",
      "Error: ", conditionMessage(e)
    )
  })
}
