# =============================================================================
# S3 method: printing wri_catalog objects
# =============================================================================

# Internal helper: classify an item id into domain + type
.wri_classify_item_id <- function(id, domains) {
  id0 <- tolower(id)

  dom <- domains[vapply(domains, function(d) startsWith(id0, paste0(d, "_")), logical(1))]
  if (length(dom) == 0) return(list(domain = NA_character_, kind = "other", label = NA_character_))
  dom <- dom[[1]]

  tail <- sub(paste0("^", dom, "_"), "", id0)

  domain_level <- c("domain_score", "resilience", "resistance", "recovery")
  if (tail %in% domain_level) {
    return(list(domain = dom, kind = "domain_level", label = tail))
  }

  list(domain = dom, kind = "indicator", label = tail)
}

#' @export
print.wri_catalog <- function(x, ...) {

  domains <- c(
    "air_quality", "communities", "infrastructure", "livelihoods",
    "natural_habitats", "sense_of_place", "species", "water"
  )
  domain_level <- c("domain_score", "resilience", "resistance", "recovery")

  cat("WRI STAC Catalog\n")
  cat("----------------\n")
  cat("Catalog path:", x$path, "\n")
  if (!is.null(x$built_at)) cat("Built at:    ", format(x$built_at), "\n")

  cols <- x$data$collections
  if (is.null(cols) || length(cols) == 0) {
    cat("\nCollections: 0\n")
    return(invisible(x))
  }

  cat("\nCollection:", names(cols)[1], "\n")

  item_ids <- names(x$data$items)
  if (length(item_ids) == 0) {
    cat("\nItems: 0\n")
    return(invisible(x))
  }

  cls <- lapply(item_ids, .wri_classify_item_id, domains = domains)
  dom <- vapply(cls, `[[`, character(1), "domain")
  kind <- vapply(cls, `[[`, character(1), "kind")
  label <- vapply(cls, `[[`, character(1), "label")

  cat("\nDomains (", length(domains), "):\n", sep = "")

  for (d in domains) {
    d_idx <- which(dom == d)

    if (length(d_idx) == 0) {
      cat("  - ", d, ": no layers found\n", sep = "")
      next
    }

    dl <- label[d_idx][kind[d_idx] == "domain_level"]
    present <- domain_level %in% dl

    ind <- label[d_idx][kind[d_idx] == "indicator"]
    n_ind <- length(ind)

    cat("  - ", d, "\n", sep = "")
    cat(
      "      domain-level: ",
      paste0(
        c("score", "resilience", "resistance", "recovery"),
        "=",
        c(present[1], present[2], present[3], present[4])
      ) |> paste(collapse = ", "),
      "\n",
      sep = ""
    )
    cat("      indicators:   ", n_ind, "\n", sep = "")
  }

  other_idx <- which(is.na(dom) | kind == "other")
  if (length(other_idx) > 0) {
    cat("\nUnclassified items (do not match domain_* pattern): ", length(other_idx), "\n", sep = "")
    show <- head(item_ids[other_idx], 10)
    cat("  ", paste(show, collapse = ", "), if (length(other_idx) > 10) ", ..." else "", "\n", sep = "")
  }

  invisible(x)
}
