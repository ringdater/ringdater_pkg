context("loaded_data_check")
library(ringdater)

test_that("loaded_data_check() evaluates the data loaded inthe the shiny app and returns an error code", {
  test1 <- c(1:10)
  expect_error(loaded_data_check(test1))

  test2 <- data.frame(x = 1:10)
  expect_error(loaded_data_check(test2))

  test3 <- data.frame(x = 1:10, y = 1:10)
  expect_equal(loaded_data_check(test3), 0)

  test4 <- data.frame(x = rep(NA,10), y = 1:10)
  expect_equal(loaded_data_check(test4), 1)

  test5 <- data.frame(x = 1:10, y = c(1:4,NA,6:10))
  expect_equal(loaded_data_check(test5), 2)
})

