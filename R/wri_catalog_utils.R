# =============================================================================
# STAC Helper Functions (Internal) — devtools::load_all() workflow
#
# ASSUMPTIONS:
# - Dev workflow (not installed) is OK
# - STAC is link-navigable:
#     catalog.json -> rel="child" to collection.json
#     collection.json -> rel="item" to items/*.json
# =============================================================================

# -----------------------------------------------------------------------------
# GET PACKAGE PATH (DEV MODE)
# -----------------------------------------------------------------------------

wri_get_pkg_path <- function() {

  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop(
      "Dev mode required: install 'pkgload' and load with devtools::load_all().\n",
      "Install: install.packages('pkgload')",
      call. = FALSE
    )
  }

  # In a devtools::load_all() session, pkgload knows the current package path.
  pkg_path <- tryCatch(pkgload::pkg_path(), error = function(e) NA_character_)

  if (!is.na(pkg_path) && nzchar(pkg_path) && dir.exists(pkg_path)) {
    return(normalizePath(pkg_path, winslash = "/", mustWork = TRUE))
  }

  # If you sourced this file directly, pkgload may not be initialized; give a clear error.
  stop(
    "Could not determine package path in dev mode.\n\n",
    "Run this from the package root (folder with DESCRIPTION):\n",
    "  setwd('~/MEDS/Capstone/firex')\n",
    "  devtools::load_all()\n",
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# RESOLVE STAC CATALOG PATH
# -----------------------------------------------------------------------------

wri_resolve_stac_path <- function() {

  pkg_path <- wri_get_pkg_path()

  # Expected layout:
  #   <project_root>/firex/ (pkg_path)
  #   <project_root>/wri-data-processing/stac/catalog.json
  project_root <- normalizePath(file.path(pkg_path, ".."), winslash = "/", mustWork = FALSE)

  stac_path <- normalizePath(
    file.path(project_root, "wri-data-processing", "stac", "catalog.json"),
    winslash = "/",
    mustWork = FALSE
  )

  if (!file.exists(stac_path)) {
    stop(
      "STAC catalog not found.\n\n",
      "I looked for:\n  ", stac_path, "\n\n",
      "Expected layout:\n",
      "  <project_root>/firex/\n",
      "  <project_root>/wri-data-processing/stac/catalog.json\n",
      call. = FALSE
    )
  }

  stac_path
}

# -----------------------------------------------------------------------------
# RESOLVE HREF (RELATIVE / ABSOLUTE / URL)
# -----------------------------------------------------------------------------

wri_resolve_href <- function(base_file, href) {

  if (is.null(href) || !nzchar(href)) {
    return(NA_character_)
  }

  # URL (future-proofing)
  if (grepl("^[a-zA-Z]+://", href)) {
    return(href)
  }

  # Absolute filesystem path
  if (grepl("^/", href)) {
    return(normalizePath(href, winslash = "/", mustWork = FALSE))
  }

  # Relative path (relative to the directory containing base_file)
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
      href = if (!is.null(a$href)) wri_resolve_href(item_file, a$href) else NA_character_,
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
    assign(path, obj, envir = visited)  # <-- FIX: assign() not set()
    obj
  }

  catalog <- read_file(catalog_path)

  # Catalog children (collections)
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

    col_id <- if (!is.null(col$id) && nzchar(col$id)) col$id else basename(dirname(cp))

    # With your updated collection.json, rel="item" exists and href looks like "items/<id>.json"
    item_links <- c(
      wri_links_by_rel(col, "item"),
      wri_links_by_rel(col, "child")
    )

    item_paths <- unique(
      vapply(item_links,
             function(l) wri_resolve_href(cp, l$href),
             character(1))
    )

    if (length(item_paths) == 0) {
      stop(
        "No items discovered for collection '", col_id, "'.\n",
        "Expected rel='item' links in:\n  ", cp,
        call. = FALSE
      )
    }

    col_item_ids <- character()

    for (ip in item_paths) {

      if (is.na(ip) || !file.exists(ip)) next

      it <- read_file(ip)

      # Keep only STAC Items
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
