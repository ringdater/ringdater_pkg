context("correl_replace")
library(ringdater)

test_that("correl_replace() calculates correlations between individual measurement series and chronology with replacement", {
  test1 <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
  the_data<-load_chron(test1)
  expect_equal(nrow(correl_replace(the_data)),(ncol(the_data)-1))
  expect_equal(ncol(correl_replace(the_data)),6)
})
