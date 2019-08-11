context("pull_dataset")

test_that("Dataset must returns the correct output", {
  skip_on_cran()
  set_rhdx_config()
  expect_is(pull_dataset("reliefweb-crisis-figures"),
            "Dataset")
})
