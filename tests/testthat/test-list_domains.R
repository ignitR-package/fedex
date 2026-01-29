test_that("list_domains returns expected domains", {
  domains <- list_domains()

  expect_type(domains, "character")
  expect_length(domains, 8)
  expect_true("water" %in% domains)
  expect_true("infrastructure" %in% domains)
})
