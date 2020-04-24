context("pull_resource")

test_that("Resource must returns the correct output", {
  skip_on_cran()
  skip_if_offline()
  set_rhdx_config()
  expect_is(pull_resource("09cbc3d9-33d4-428e-8ed0-e5b157fcd0ee"), "Resource")
})
