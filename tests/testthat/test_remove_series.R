context("remove_series")
library(ringdater)

test_that("remove_series removes time series from a data.frame based on series ID", {
  undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
  undated_data <- load_undated(undated_path)
  samples <- colnames(undated_data )[c(2,3,6)]
  remove_series(undated_data, samples)

  expect_equal(ncol(remove_series(undated_data, samples)), 11)
  expect_equal(nrow(remove_series(undated_data, samples)), 391)

  expect_error(remove_series(the.data = undated_data, series.id = NULL))
  expect_error(remove_series(the.data = c(1:50), series.id = samples))

})
