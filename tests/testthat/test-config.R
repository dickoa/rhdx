context("set_rhdx_config")

test_that("create_rhdx_config must returns a Configuration object", {
  expect_is(create_rhdx_config(), "Configuration")
})

test_that("Set configuration on the wrong server", {
  skip_on_cran()
  expect_error(set_rhdx_config(hdx_site = "fake"))
})

test_that("API key must be valid i.e uuid", {
  skip_on_cran()
  expect_error(set_rhdx_config(hdx_key = "abcdefd"))
})
