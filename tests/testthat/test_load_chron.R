context("load_chron")
library(ringdater)

test_that("load_chron() loads data files containing undated measurement timeseries", {
  test1 <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
  expect_equal(ncol(load_chron(test1)), 14)
  expect_equal(nrow(load_chron(test1)), 329)
  expect_equal(class(load_chron(test1)), "data.frame")
})
