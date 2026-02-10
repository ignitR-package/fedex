#' Query STAC items flexibly by any property
#'
#' @param items List of STAC items
#' @param ... Named property-value pairs to filter by
#' @return Filtered list of items
#' @export
#' @examples
#' items <- load_stac_items()
#' query_stac_flexible(items, wri_domain = "water", data_type = "status")
query_stac_flexible <- function(items, ...) {
  filters <- list(...)
  Filter(function(x) {
    all(sapply(names(filters), function(prop) {
      !is.null(x$properties[[prop]]) && x$properties[[prop]] == filters[[prop]]
    }))
  }, items)
}