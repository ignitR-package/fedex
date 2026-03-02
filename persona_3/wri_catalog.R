# =============================================================================
# Public: Catalog access
# =============================================================================

#' Read the WRI STAC catalog
#'
#' Reads the local STAC catalog (fresh by default) and returns a `wri_catalog`
#' object that can be inspected and used by other package functions.
#'
#' @param fresh Logical. If TRUE (default), re-reads the STAC from disk each time.
#'   (Currently, fresh=FALSE is not implemented; the catalog is always rebuilt.)
#'
#' @return An object of class `wri_catalog`.
#' @export
wri_catalog <- function(fresh = TRUE) {

  # fresh currently always behaves as TRUE (local STAC may change frequently)
  data <- wri_read_stac_tree()

  structure(
    list(
      path = data$catalog_path,
      built_at = Sys.time(),
      data = data
    ),
    class = "wri_catalog"
  )
}
