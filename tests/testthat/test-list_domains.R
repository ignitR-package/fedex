test_that("list_domains returns expected domains", {
  domains <- list_domains()

  expect_type(domains, "character")
  expect_true(length(domains) > 0)
  expect_equal(domains, sort(unique(domains)))
})
