test_that("query returns a basic layer table", {
  layers <- list_layers()

  expect_s3_class(layers, "data.frame")
  expect_true(nrow(layers) > 0)
  expect_true(all(c("id", "domain", "dimension", "data_type", "is_hosted", "href") %in% names(layers)))
})

test_that("query filters hosted layers", {
  hosted <- list_layers(hosted_only = TRUE)

  expect_s3_class(hosted, "data.frame")
  expect_true(nrow(hosted) > 0)
  expect_true(all(hosted$is_hosted))
})

test_that("explore helpers return expected shapes", {
  domains <- list_domains()
  dimensions <- list_dimensions()
  data_types <- list_data_types()

  expect_type(domains, "character")
  expect_type(dimensions, "character")
  expect_type(data_types, "character")
  expect_true(length(domains) > 0)
})

test_that("search_layers can find known IDs by pattern", {
  layers <- list_layers()
  known_id <- layers$id[[1]]
  pattern <- substr(known_id, 1, 4)

  matches <- search_layers(pattern)
  expect_s3_class(matches, "data.frame")
  expect_true(known_id %in% matches$id)
})

test_that("retrieve validates inputs before attempting download", {
  expect_error(get_layer(), "Layer 'id' is required")
  expect_error(
    get_layer("definitely_not_a_real_layer_id"),
    "not found in STAC catalog"
  )
  expect_error(
    get_layer(id = "WRI_score", bbox = c(-122, 37, -121)),
    "'bbox' must be a numeric vector of length 4"
  )
})
