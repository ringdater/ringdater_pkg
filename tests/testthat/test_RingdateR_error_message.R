context("RingdateR_error_message")
library(ringdater)

test_that("RingdateR_error_message produces error messages for the shiny app", {

  class(RingdateR_error_message(message="Can't display plot", plot.err = TRUE))

  expect_equal(class(RingdateR_error_message(message="Can't display plot", plot.err = TRUE))[1], "gg")
  expect_error(RingdateR_error_message(message= 1234, plot.err = TRUE))
  expect_error(RingdateR_error_message(message= "Can't display plot", plot.err = "not a bool"))
})
