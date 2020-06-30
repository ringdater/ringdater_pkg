context("name_check")
library(ringdater)

test_that("name_check modifies sample names to conform to requirements of ringdater", {

  the_data <-data.frame(x=1:10, y= rnorm(10, 1, 2))
  colnames(the_data)<- c("x.123", "x.456")
  test_names<-name_check(the_data)
  expect_equal(colnames(test_names)[1], "ID__123")
  expect_equal(colnames(test_names)[2], "ID__456")
})
