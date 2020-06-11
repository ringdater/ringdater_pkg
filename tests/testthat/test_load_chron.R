context("load_dated")
library(ringdater)

test_that("load_dated() loads data files containing undated measurement timeseries", {
  test1 <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
  expect_equal(ncol(load_chron(test1)), 14)
  expect_equal(nrow(load_chron(test1)), 329)
})
