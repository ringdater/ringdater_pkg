context("load_undated")
library(ringdater)

test_that("load_undated() loads data files containing undated measurement timeseries", {
  path <- system.file("extdata", "undated_example.csv", package="ringdater")
  expect_equal(ncol(load_undated(path)), 14)
  expect_equal(nrow(load_undated(path)), 391)
})

