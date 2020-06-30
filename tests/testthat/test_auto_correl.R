context("auto_correl")
library(ringdater)

test_that("Auto_correl() produces a data.frame containing the auto-correlation to the 10th degree", {
  the_data <-data.frame(x=1:100, y= rnorm(100, 1, 2))
  expect_equal(ncol(auto_correl(the_data)), 2)
  expect_equal(nrow(auto_correl(the_data)), 11)
})
