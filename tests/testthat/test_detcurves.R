context("detcurves")
library(ringdater)

test_that("detcurves() return a data.frame containing detrending curves", {
  path <- system.file("extdata", "undated_example.csv", package="ringdater")
  the_data<-load_undated(path)
  detcurves(series_data = the_data, detrending_select = 3, splinewindow = 21)
  expect_equal(ncol(detcurves(series_data = the_data, detrending_select = 3, splinewindow = 21)), ncol(the_data))
  expect_equal(nrow(detcurves(series_data = the_data, detrending_select = 3, splinewindow = 21)), nrow(the_data))
})
