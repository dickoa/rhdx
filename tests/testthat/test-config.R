context("set_rhdx_config")

test_that("Set configuration on the wrong server", {
  skip_on_cran()
  expect_error(set_rhdx_config(hdx_site = "fake"))
})

test_that("API key must be valid i.e uuid", {
  skip_on_cran()
  expect_error(set_rhdx_config(hdx_key = "abcdefd"))
})
