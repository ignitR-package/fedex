#' List available WRI domains
#'
#' @returns Character vector of domain names
#' @export
#'
#' @examples
#' list_domains()
list_domains <- function() {
  layers <- list_layers()
  sort(unique(layers$domain[!is.na(layers$domain)]))
}


#' List available dimensions
#'
#' @returns Character vector of dimension names
#' @export
#'
#' @examples
#' list_dimensions()
list_dimensions <- function() {
  layers <- list_layers()
  sort(unique(layers$dimension[!is.na(layers$dimension)]))
}


#' List available data types
#'
#' @returns Character vector of data type names
#' @export
#'
#' @examples
#' list_data_types()
list_data_types <- function() {
  layers <- list_layers()
  sort(unique(layers$data_type[!is.na(layers$data_type)]))
}


#' Search for layers by name pattern
#'
#' @param pattern Character. Pattern to search for in layer IDs (case-insensitive)
#'
#' @returns Data frame of matching layers
#' @export
#'
#' @examples
#' # Find all layers with "fire" in the name
#' search_layers("fire")
#'
#' # Find asthma-related layers
#' search_layers("asthma")
search_layers <- function(pattern) {
  layers <- list_layers()
  matches <- grepl(pattern, layers$id, ignore.case = TRUE)
  layers[matches, ]
}


#' Print catalog summary
#'
#' @returns NULL (prints summary to console)
#' @export
#'
#' @examples
#' catalog_summary()
catalog_summary <- function() {
  layers <- list_layers()

  cat("\n=== WRI Data Catalog Summary ===\n\n")
  cat("Total layers:    ", nrow(layers), "\n")
  cat("Hosted on KNB:   ", sum(layers$is_hosted), "\n")
  cat("Local only:      ", sum(!layers$is_hosted), "\n\n")

  cat("Domains (", length(unique(layers$domain[!is.na(layers$domain)])), "):\n", sep = "")
  cat("  ", paste(sort(unique(layers$domain[!is.na(layers$domain)])), collapse = ", "), "\n\n")

  cat("Dimensions (", length(unique(layers$dimension[!is.na(layers$dimension)])), "):\n", sep = "")
  cat("  ", paste(sort(unique(layers$dimension[!is.na(layers$dimension)])), collapse = ", "), "\n\n")

  cat("Data types (", length(unique(layers$data_type)), "):\n", sep = "")
  cat("  ", paste(sort(unique(layers$data_type)), collapse = ", "), "\n\n")

  invisible(NULL)
}
