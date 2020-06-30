context("curve_data")
library(ringdater)

test_that("curve_data generates the detrending curves used to dentredn the timeseries", {
  path <- system.file("extdata", "undated_example.csv", package="ringdater")
  the_data<-load_undated(path)
  curve_data <- detcurves(series_data = the_data, detrending_select = 3, splinewindow = 21)

  expect_equal(ncol(curve_data), 14)
  expect_equal(nrow(curve_data), 391)
  expect_equal(class(curve_data), "data.frame")
})
