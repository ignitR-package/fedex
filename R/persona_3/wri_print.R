# =============================================================================
# S3 method: printing wri_catalog objects (uses STAC summaries + item properties)
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

  cat("WRI STAC Catalog\n")
  cat("----------------\n")
  cat("Catalog path:", x$path, "\n")
  if (!is.null(x$built_at)) cat("Built at:    ", format(x$built_at), "\n")

  cols <- x$data$collections
  if (is.null(cols) || length(cols) == 0) {
    cat("\nCollections: 0\n")
    return(invisible(x))
  }

  # Current structure: assume single collection
  col_id <- names(cols)[1]
  col_obj <- cols[[1]]$collection

  cat("\nCollection:", col_id, "\n")

  # Pull authoritative structure from collection summaries
  sums <- col_obj$summaries
  if (is.null(sums)) {
    cat("\nNo 'summaries' found in collection.json. Cannot print structured summary.\n")
    return(invisible(x))
  }

  data_type_vals <- unname(unlist(sums$data_type %||% character()))
  domain_vals    <- unname(unlist(sums$wri_domain %||% character()))
  dim_vals       <- unname(unlist(sums$wri_dimension %||% character()))

  cat("\nStructure (from collection summaries):\n")
  cat("  data_type:     ", paste(data_type_vals, collapse = ", "), "\n", sep = "")
  cat("  wri_domain:    ", paste(domain_vals, collapse = ", "), "\n", sep = "")
  cat("  wri_dimension: ", paste(dim_vals, collapse = ", "), "\n", sep = "")
  cat("\n")

  # Items
  item_ids <- names(x$data$items)
  if (length(item_ids) == 0) {
    cat("Items: 0\n")
    return(invisible(x))
  }
  cat("Items: ", length(item_ids), "\n\n", sep = "")

  # Classify each item strictly from item properties
  item_dom <- character(length(item_ids))
  item_dim <- character(length(item_ids))
  item_typ <- character(length(item_ids))

  for (i in seq_along(item_ids)) {
    it <- x$data$items[[ item_ids[[i]] ]]$item
    item_dom[[i]] <- .wri_prop1(it, "wri_domain")
    item_dim[[i]] <- .wri_prop1(it, "wri_dimension")
    item_typ[[i]] <- .wri_prop1(it, "data_type")
  }

  # Print domain × dimension counts in the exact summaries order
  cat("Layers by wri_domain × wri_dimension:\n")

  for (d in domain_vals) {
    idx_d <- which(item_dom == d)

    if (length(idx_d) == 0) {
      cat("  - ", d, ": 0\n", sep = "")
      next
    }

    cat("  - ", d, " (", length(idx_d), ")\n", sep = "")

    for (m in dim_vals) {
      n <- sum(item_dom == d & item_dim == m, na.rm = TRUE)
      if (n > 0) cat("      - ", m, ": ", n, "\n", sep = "")
    }

    miss_dim <- sum(item_dom == d & !(item_dim %in% dim_vals), na.rm = TRUE)
    if (miss_dim > 0) {
      cat("      - (missing/other dimension): ", miss_dim, "\n", sep = "")
    }
  }

  # data_type counts
  cat("\nLayers by data_type:\n")
  for (t in data_type_vals) {
    cat("  - ", t, ": ", sum(item_typ == t, na.rm = TRUE), "\n", sep = "")
  }
  miss_typ <- sum(is.na(item_typ) | !nzchar(item_typ), na.rm = TRUE)
  if (miss_typ > 0) cat("  - (missing data_type): ", miss_typ, "\n", sep = "")
  other_typ <- sum(!(item_typ %in% data_type_vals) & nzchar(item_typ), na.rm = TRUE)
  if (other_typ > 0) cat("  - (other data_type): ", other_typ, "\n", sep = "")

  # Report unclassified items (missing properties or not in summaries)
  bad <- which(
    is.na(item_dom) | !nzchar(item_dom) | !(item_dom %in% domain_vals) |
      is.na(item_dim) | !nzchar(item_dim) | !(item_dim %in% dim_vals)
  )

  if (length(bad) > 0) {
    cat("\nUnclassified / mismatched items: ", length(bad), "\n", sep = "")
    show <- head(item_ids[bad], 12)
    cat("  ", paste(show, collapse = ", "), if (length(bad) > 12) ", ..." else "", "\n", sep = "")
    cat("  (These items are missing wri_domain/wri_dimension properties or use values not listed in summaries.)\n")
  }

  invisible(x)
}
