#' List all unique property names and values in STAC items
#'
#' @param items List of STAC items
#' @return Named list of unique values for each property
#' @export
#' @examples
#' items <- load_stac_items()
#' list_stac_properties(items)
list_stac_properties <- function(items) {
  all_props <- unique(unlist(lapply(items, function(x) names(x$properties))))
  res <- setNames(vector("list", length(all_props)), all_props)
  for (prop in all_props) {
    res[[prop]] <- unique(sapply(items, function(x) x$properties[[prop]]))
  }
  res
}

#' Summarize all STAC items in a data frame
#'
#' @param items List of STAC items
#' @param properties Character vector of property names to include (default: all)
#' @return Data frame summary
#' @export
#' @examples
#' items <- load_stac_items()
#' summarize_stac_items(items, c("id", "wri_domain", "data_type"))
summarize_stac_items <- function(items, properties = NULL) {
  if (is.null(properties)) {
    properties <- unique(unlist(lapply(items, function(x) names(x$properties))))
  }
  df <- data.frame(id = sapply(items, function(x) x$id), stringsAsFactors = FALSE)
  for (prop in properties) {
    df[[prop]] <- sapply(items, function(x) x$properties[[prop]])
  }
  df
}
#' List all STAC item files
#'
#' @return List of item file paths
#' @export
#' @examples
#' files <- list_stac_item_files()
#' head(files)
list_stac_item_files <- function() {
  items_dir <- get_stac_catalog_path()
  list.files(items_dir, pattern = "\\.json$", full.names = TRUE)
}


#' Load all STAC items
#'
#' @return List of STAC item objects
#' @export
#' @examples
#' items <- load_stac_items()
#' length(items)
load_stac_items <- function() {
  item_files <- list_stac_item_files()
  lapply(item_files, jsonlite::fromJSON, simplifyVector = FALSE)
}



#' (Deprecated) Extract unique domains from STAC items
#'
#' @param items List of STAC items
#' @return Character vector of unique domains
#' @export
#' @examples
#' items <- load_stac_items()
#' list_stac_properties(items)$wri_domain
get_domains <- function(items) {
  .Deprecated("list_stac_properties")
  unique(sapply(items, function(x) x$properties$wri_domain))
}



#' (Deprecated) Extract unique layer types from STAC items
#'
#' @param items List of STAC items
#' @return Character vector of unique layer types
#' @export
#' @examples
#' items <- load_stac_items()
#' list_stac_properties(items)$wri_layer_type
get_layer_types <- function(items) {
  .Deprecated("list_stac_properties")
  unique(sapply(items, function(x) x$properties$wri_layer_type))
}



#' (Deprecated) Extract unique data types from STAC items
#'
#' @param items List of STAC items
#' @return Character vector of unique data types
#' @export
#' @examples
#' items <- load_stac_items()
#' list_stac_properties(items)$data_type
get_data_types <- function(items) {
  .Deprecated("list_stac_properties")
  unique(sapply(items, function(x) x$properties$data_type))
}


#' Filter STAC items by property
#'
#' @param items List of STAC items
#' @param property Property name
#' @param value Value to match
#' @return Filtered list of items
#' @export
#' @examples
#' items <- load_stac_items()
#' filter_items_by_property(items, "wri_domain", "livelihoods")
filter_items_by_property <- function(items, property, value) {
  Filter(function(x) x$properties[[property]] == value, items)
}


#' Get asset URLs from a STAC item
#'
#' @param item A STAC item
#' @param asset_type Optional asset type (e.g., "data")
#' @return Character vector of asset URLs
#' @export
#' @examples
#' items <- load_stac_items()
#' get_asset_urls(items[[1]])
get_asset_urls <- function(item, asset_type = NULL) {
  assets <- item$assets
  if (!is.null(asset_type)) {
    assets <- assets[asset_type]
  }
  sapply(assets, function(a) a$href)
}
