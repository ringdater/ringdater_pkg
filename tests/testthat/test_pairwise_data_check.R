context("pairwise_data_check")
library(ringdater)

test_that("pairwise_data_check checks the loaded data for potential errors and returns the dataframe if all is OK", {
  the_data <-data.frame(x=1:100, y= rnorm(100, 1, 2))
  expect_equal(ncol(pairwise_data_check(the_data)), 2)
  expect_equal(nrow(pairwise_data_check(the_data)), 100)

  expect_error(pairwise_data_check(c(1:100)))
})
