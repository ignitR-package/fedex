# =============================================================================
# S3 method: printing wri_catalog objects (summary view)
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Internal helper: safely pull a scalar property from a STAC item
.wri_prop1 <- function(item_obj, key) {
  p <- item_obj$properties
  if (is.null(p)) return(NA_character_)
  val <- p[[key]]
  if (is.null(val) || length(val) == 0) return(NA_character_)
  as.character(val[[1]])
}

#' @export
print.wri_catalog <- function(x, ...) {

  cat("WRI DATA SUMMARY\n")
  cat("-------------------\n")

  if (!is.null(x$built_at)) {
    cat("Read at:      ", format(x$built_at), "\n", sep = "")
  }

  cols <- x$data$collections
  if (is.null(cols) || length(cols) == 0) {
    cat("\nCollections: 0\n")
    return(invisible(x))
  }

  collection_ids <- names(cols)
  cat("\nCollections (", length(collection_ids), "): ",
      paste(collection_ids, collapse = ", "), "\n", sep = "")

  col_obj <- cols[[1]]$collection
  sums <- col_obj$summaries

  if (is.null(sums)) {
    cat("\nNo collection summaries found.\n")
    return(invisible(x))
  }

  data_type_vals <- sort(unique(unname(unlist(sums$data_type %||% character()))))
  domain_vals    <- sort(unique(unname(unlist(sums$wri_domain %||% character()))))
  dim_vals       <- sort(unique(unname(unlist(sums$wri_dimension %||% character()))))

  cat("Unique data_type values (", length(data_type_vals), "): ",
      paste(data_type_vals, collapse = ", "), "\n", sep = "")
  cat("Unique wri_domain values (", length(domain_vals), "): ",
      paste(domain_vals, collapse = ", "), "\n", sep = "")
  cat("Unique wri_dimension values (", length(dim_vals), "): ",
      paste(dim_vals, collapse = ", "), "\n", sep = "")

  item_ids <- names(x$data$items)
  cat("Items: ", length(item_ids), "\n", sep = "")

  invisible(x)
}
