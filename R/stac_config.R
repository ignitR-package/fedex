# Reference: R Packages (2e), Chapter 7.4: Internal data
#   https://r-pkgs.org/data.html#sec-data-extdata

#' Get the STAC catalog path
#'
#' Returns the path to the STAC catalog shipped with the fedex package.
#' The STAC catalog contains metadata about all available WRI layers.
#'
#' @return Character. Full path to the STAC items directory
#'
#' @details
#' The STAC catalog is pre-generated and ships with the package in
#' inst/extdata/stac/. This function uses system.file() to find the
#' installed location.
#'
#' @examples
#' # Get the path to the STAC catalog
#' stac_path <- get_stac_catalog_path()
#'
#' # List STAC item files
#' list.files(stac_path, pattern = "\\.json$")
#'
#' @export
get_stac_catalog_path <- function() {
  # Use system.file() to find installed extdata location
  # This works whether package is installed or loaded with devtools::load_all()
  system.file("extdata", "stac", "collections", "wri_ignitR", "items",
              package = "fedex")
}
