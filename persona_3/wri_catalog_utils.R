# =============================================================================
# STAC Helper Functions (Internal)
#
# These functions:
# - Resolve package and STAC paths
# - Traverse the STAC structure (catalog -> collections -> items)
# - Extract and resolve asset paths
#
# These are INTERNAL helpers and should not be exported.
# =============================================================================


# -----------------------------------------------------------------------------
# GET PACKAGE PATH (INSTALLED OR DEVTOOLS LOAD_ALL)
# -----------------------------------------------------------------------------

wri_get_pkg_path <- function() {

  # Case 1: Installed package
  pkg_path <- system.file(package = "fedex")
  if (nzchar(pkg_path)) {
    return(normalizePath(pkg_path))
  }

  # Case 2: Development mode (devtools::load_all)
  if (requireNamespace("pkgload", quietly = TRUE)) {
    dev_path <- tryCatch(
      pkgload::pkg_path("fedex"),
      error = function(e) NA_character_
    )

    if (!is.na(dev_path) && nzchar(dev_path)) {
      return(normalizePath(dev_path))
    }
  }

  stop(
    "Could not determine package path. ",
    "Install the package or load it with devtools::load_all().",
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# RESOLVE STAC CATALOG PATH
# -----------------------------------------------------------------------------

wri_resolve_stac_path <- function() {

  pkg_path <- wri_get_pkg_path()

  project_root <- normalizePath(file.path(pkg_path, ".."))

  stac_path <- file.path(
    project_root,
    "wri-data-processing",
    "stac",
    "catalog.json"
  )

  if (!file.exists(stac_path)) {
    stop(
      "STAC catalog not found at expected location:\n",
      stac_path,
      call. = FALSE
    )
  }

  normalizePath(stac_path)
}


# -----------------------------------------------------------------------------
# RESOLVE HREF (RELATIVE / ABSOLUTE / URL)
# -----------------------------------------------------------------------------

wri_resolve_href <- function(base_file, href) {

  if (is.null(href) || !nzchar(href)) {
    return(NA_character_)
  }

  # URL (future-proofing for KNB)
  if (grepl("^[a-zA-Z]+://", href)) {
    return(href)
  }

  # Absolute filesystem path
  if (grepl("^/", href)) {
    return(normalizePath(href, winslash = "/", mustWork = FALSE))
  }

  # Relative path
  normalizePath(
    file.path(dirname(base_file), href),
    winslash = "/",
    mustWork = FALSE
  )
}


# -----------------------------------------------------------------------------
# FILTER LINKS BY REL TYPE
# -----------------------------------------------------------------------------

wri_links_by_rel <- function(stac_obj, rel) {

  links <- stac_obj$links
  if (is.null(links) || length(links) == 0) {
    return(list())
  }

  keep <- vapply(
    links,
    function(x) identical(x$rel, rel),
    logical(1)
  )

  links[keep]
}


# -----------------------------------------------------------------------------
# EXTRACT ASSETS FROM STAC ITEM (TIDY DATA FRAME)
# -----------------------------------------------------------------------------

wri_item_assets_df <- function(item_obj, item_file) {

  assets <- item_obj$assets

  if (is.null(assets) || length(assets) == 0) {
    return(data.frame(
      asset_name = character(),
      href = character(),
      type = character(),
      roles = I(list())
    ))
  }

  out <- lapply(names(assets), function(nm) {

    a <- assets[[nm]]

    data.frame(
      asset_name = nm,
      href = if (!is.null(a$href))
        wri_resolve_href(item_file, a$href)
      else
        NA_character_,
      type = if (!is.null(a$type)) a$type else NA_character_,
      roles = I(list(if (!is.null(a$roles)) a$roles else character()))
    )
  })

  do.call(rbind, out)
}


# -----------------------------------------------------------------------------
# READ ROOT STAC (MINIMAL)
# -----------------------------------------------------------------------------

wri_read_stac_root <- function() {

  if (!requireNamespace("rstac", quietly = TRUE)) {
    stop(
      "Package 'rstac' is required. Install it with install.packages('rstac').",
      call. = FALSE
    )
  }

  stac_path <- wri_resolve_stac_path()
  rstac::read_stac(stac_path)
}


# -----------------------------------------------------------------------------
# READ FULL STAC TREE (CATALOG -> COLLECTIONS -> ITEMS)
# -----------------------------------------------------------------------------

wri_read_stac_tree <- function() {

  if (!requireNamespace("rstac", quietly = TRUE)) {
    stop(
      "Package 'rstac' is required. Install it with install.packages('rstac').",
      call. = FALSE
    )
  }

  catalog_path <- wri_resolve_stac_path()

  visited <- new.env(parent = emptyenv())

  read_file <- function(path) {

    path <- normalizePath(path, winslash = "/", mustWork = FALSE)

    if (exists(path, envir = visited, inherits = FALSE)) {
      return(get(path, envir = visited, inherits = FALSE))
    }

    obj <- rstac::read_stac(path)
    set(path, obj, envir = visited)
    obj
  }

  catalog <- read_file(catalog_path)

  # Catalog children (typically collections)
  child_links <- wri_links_by_rel(catalog, "child")
  child_paths <- unique(
    vapply(child_links,
           function(l) wri_resolve_href(catalog_path, l$href),
           character(1))
  )

  collections <- list()
  items <- list()

  for (cp in child_paths) {

    if (is.na(cp) || !file.exists(cp)) next

    col <- read_file(cp)

    col_id <- if (!is.null(col$id) && nzchar(col$id))
      col$id
    else
      basename(dirname(cp))

    item_links <- c(
      wri_links_by_rel(col, "item"),
      wri_links_by_rel(col, "child")
    )

    item_paths <- unique(
      vapply(item_links,
             function(l) wri_resolve_href(cp, l$href),
             character(1))
    )

    col_item_ids <- character()

    for (ip in item_paths) {

      if (is.na(ip) || !file.exists(ip)) next

      it <- read_file(ip)

      if (!is.null(it$type) && !identical(it$type, "Feature")) next

      it_id <- it$id
      if (is.null(it_id) || !nzchar(it_id)) {
        it_id <- tools::file_path_sans_ext(basename(ip))
      }

      items[[it_id]] <- list(
        id = it_id,
        collection = col_id,
        item_path = ip,
        item = it,
        assets = wri_item_assets_df(it, ip)
      )

      col_item_ids <- c(col_item_ids, it_id)
    }

    collections[[col_id]] <- list(
      id = col_id,
      collection_path = cp,
      collection = col,
      item_ids = unique(col_item_ids)
    )
  }

  list(
    catalog_path = catalog_path,
    catalog = catalog,
    collections = collections,
    items = items
  )
}
