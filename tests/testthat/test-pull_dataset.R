context("pull_dataset")

test_that("pull_dataset must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  set_rhdx_config()
  expect_is(pull_dataset("reliefweb-crisis-figures"), "Dataset")
})
