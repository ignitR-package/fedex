## These tests are designed for a local STAC catalog by default.
## To use a remote STAC API, adjust get_stac_catalog_path() and related functions accordingly.

test_that("STAC catalog path is correct (local)", {
  # For remote, replace with a URL and adjust logic in get_stac_catalog_path()
  expect_true(dir.exists(get_stac_catalog_path()) || file.exists(get_stac_catalog_path()))
})

test_that("STAC item files can be listed (local)", {
  # For remote, implement a function to list items from the API
  files <- list_stac_item_files()
  expect_type(files, "character")
  expect_true(all(grepl("\\.json$", files)))
})

test_that("STAC items can be loaded (local)", {
  # For remote, implement a function to load items from the API
  items <- load_stac_items()
  expect_type(items, "list")
  expect_true(length(items) > 0)
})

test_that("Domain extraction works", {
  items <- load_stac_items()
  domains <- get_domains(items)
  expect_type(domains, "character")
})

test_that("Layer type extraction works", {
  items <- load_stac_items()
  types <- get_layer_types(items)
  expect_type(types, "character")
})

test_that("Data type extraction works", {
  items <- load_stac_items()
  types <- get_data_types(items)
  expect_type(types, "character")
})

test_that("Asset URL extraction works", {
  items <- load_stac_items()
  item <- items[[1]]
  urls <- get_asset_urls(item)
  expect_type(urls, "character")
})
