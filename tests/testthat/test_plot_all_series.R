context("plot_all_series")
library(ringdater)

test_that("plot_all_series generates a plot of all the aligned measurement timeseries with an arimethic chronology", {

  chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
  chron_data  <- load_chron(chron_path)
  chrono_det  <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)
  expect_equal(class(plot_all_series(aligned_data = chrono_det))[1], "gg")

  # errorr if the data loaded is not a dataframe
  expect_error(plot_all_series(aligned_data = c(1:50)))
  # error if there is a dataframe with insufficient data (ncols is too small)
  expect_error(plot_all_series(aligned_data = data.frame(x = 1:50)))

})
