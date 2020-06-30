context("prob_check")
library(ringdater)

test_that("prob_check returns a list of problem samples and the years containing potential problems", {
  chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
  chron_data  <- load_chron(chron_path)
  chrono_det  <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)

  expect_equal(nrow(prob_check(new.chrono = chrono_det, wind = 20)), 4)
  expect_equal(ncol(prob_check(new.chrono = chrono_det, wind = 20)), 2)

  expect_error(prob_check(new.chrono = chrono_det, wind = 4))
  expect_error(prob_check(new.chrono = chrono_det, wind = "not a number"))
  expect_error(prob_check(new.chrono = c(1:20), wind = 20))
})
