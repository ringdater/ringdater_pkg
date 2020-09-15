context("R_bar_EPS")
library(ringdater)

test_that("R_bar_EPS generates a datframe of Rbar and EPS values", {
  registerDoParallel(cores=2)
  chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
  chron_data  <- load_chron(chron_path)
  chrono_det  <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)

  expect_equal(ncol(R_bar_EPS(the.data = chrono_det, window = 25)), 5)
  expect_equal(nrow(R_bar_EPS(the.data = chrono_det, window = 25)), 17)

  expect_error(R_bar_EPS(the.data = c(1:50), window = 25))
  expect_error(R_bar_EPS(the.data = chrono_det, window = "not a number"))
  expect_error(R_bar_EPS(the.data = chrono_det, window = 5.6))
  expect_error(R_bar_EPS(the.data = chrono_det, window = -5))
})
