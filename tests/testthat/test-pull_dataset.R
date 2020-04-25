context("dataset")

test_that("pull_dataset must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  set_rhdx_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("reliefweb-crisis-figures")
  })
  expect_is(output, "Dataset")
})

test_that("get_resource must return an object of class Dataset", {
  skip_on_cran()
  skip_if_offline()
  set_rhdx_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("reliefweb-crisis-figures")
  })
  expect_is(get_resource(output, 1), "Resource")
})

test_that("get_resources must return a list of Resources", {
  skip_on_cran()
  skip_if_offline()
  set_rhdx_config()
  vcr::use_cassette("pull_dataset", {
    output <- pull_dataset("reliefweb-crisis-figures")
  })
  expect_is(get_resources(output), "resources_list")
  expect_is(get_resources(output)[[1]], "Resource")
  expect_that(length(get_resources(delete_resources(output))),
              equals(0))
})
