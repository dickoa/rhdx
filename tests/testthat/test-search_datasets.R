context("search_datasets")

test_that("Search datasets returns the correct output", {
  skip_on_cran()
  set_rhdx_config()
  expect_is(search_datasets(),
            "datasets_list")
})
