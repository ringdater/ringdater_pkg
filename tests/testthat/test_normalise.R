context("normalise")
library(ringdater)

test_that("normalise detrends measurement timeseries", {
  undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
  undated_data <- load_undated(undated_path)
  expect_equal(ncol(normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)), 14)
  expect_equal(nrow(normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)), 391)

  expect_error(normalise(the.data = undated_data, detrending_select = 3, splinewindow = 600))
  expect_error(normalise(the.data = undated_data, detrending_select = 8, splinewindow = 21))
  expect_error(normalise(the.data = c(1:20), detrending_select = 3, splinewindow = 21))

})
