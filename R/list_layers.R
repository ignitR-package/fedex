#' List available WRI layers
#'
#' Query the STAC catalog to find available layers. Filter by domain,
#' dimension, data type, or hosting status.
#'
#' @param domain Character. Filter by WRI domain ("air_quality", "communities",
#'   "infrastructure", "livelihoods", "sense_of_place", "species", "habitats",
#'   "water", or "unknown" for overall WRI score). Default NULL (all domains).
#' @param dimension Character. Filter by dimension ("resistance", "resilience",
#'   "recovery", "status", "domain_score"). Default NULL (all dimensions).
#' @param data_type Character. Filter by data type ("indicator", "aggregate",
#'   "final_score"). Default NULL (all types).
#' @param hosted_only Logical. If TRUE, return only layers hosted on KNB.
#'   Default FALSE.
#'
#' @returns A data.frame with columns: id, domain, dimension, data_type, is_hosted, href
#' @export
#'
#' @examples
#' # See all available layers
#' list_layers()
#'
#' # Find all air quality layers
#' list_layers(domain = "air_quality")
#'
#' # Find all resistance indicators
#' list_layers(dimension = "resistance", data_type = "indicator")
#'
#' # Find what's currently hosted on KNB
#' list_layers(hosted_only = TRUE)
#'
#' # Filter by multiple criteria
#' list_layers(domain = "communities", dimension = "resistance")
list_layers <- function(domain = NULL,
                        dimension = NULL,
                        data_type = NULL,
                        hosted_only = FALSE) {

  # Load all STAC items
  items <- load_stac_items()

  # Extract metadata into a data.frame
  layer_info <- lapply(items, function(item) {
    props <- item$properties
    href <- item$assets$data$href

    # Check if hosted by examining the href
    # If it starts with http:// or https://, it's hosted
    is_hosted <- grepl("^https?://", href)

    data.frame(
      id = item$id,
      domain = if (is.null(props$wri_domain)) NA_character_ else props$wri_domain,
      dimension = if (is.null(props$wri_dimension)) NA_character_ else props$wri_dimension,
      data_type = if (is.null(props$data_type)) NA_character_ else props$data_type,
      is_hosted = is_hosted,
      href = href,
      stringsAsFactors = FALSE
    )
  })

  # Combine into single data.frame
  df <- dplyr::bind_rows(layer_info)

  # Apply filters
  if (!is.null(domain)) {
    df <- df[df$domain == domain & !is.na(df$domain), ]
  }

  if (!is.null(dimension)) {
    df <- df[df$dimension == dimension & !is.na(df$dimension), ]
  }

  if (!is.null(data_type)) {
    df <- df[df$data_type == data_type & !is.na(df$data_type), ]
  }

  if (hosted_only) {
    df <- df[df$is_hosted, ]
  }

  # Reset row names
  rownames(df) <- NULL

  # Return sorted by id
  df[order(df$id), ]
}


# Helper function to load all STAC items
load_stac_items <- function() {

  # Define stac directory
  stac_dir <- system.file("extdata/stac/collections/wri_ignitR/items",
                          package = "fedex")

  # Check that it exists
  if (!dir.exists(stac_dir)) {
    stop("STAC catalog not found. Package may not be installed correctly.")
  }

  # Extract list of stac item files
  item_files <- list.files(stac_dir, pattern = "\\.json$", full.names = TRUE)

  # Check that there are items in the catalog
  if (length(item_files) == 0) {
    stop("No items in STAC catalog.")
  }

  # Read all items
  items <- lapply(item_files, jsonlite::read_json, simplifyVector = TRUE)

  items
}
