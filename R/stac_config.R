#' Get the local STAC catalog path
#'
#' @return Character path to the STAC catalog
#' @export
get_stac_catalog_path <- function() {
  file.path("stac", "collections", "wri_ignitR", "items")
}
