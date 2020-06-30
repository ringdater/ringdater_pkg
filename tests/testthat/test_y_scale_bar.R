context("y.scale.bar")
library(ringdater)

test_that("y.scale.bar generates a series of numbers of be used as breaks in ggplot graphs", {

  expect_equal(length(y.scale.bar(y.min = 0,y.max = 500)), 26)
  expect_equal(length(y.scale.bar(y.min = 0,y.max = 50)), 11)

  expect_error(y.scale.bar(y.min = 100,y.max = -50))
  expect_error(y.scale.bar(y.min = "not_a_num",y.max = 100))
  expect_error(y.scale.bar(y.min = 100,y.max = "not_a_num"))
})
