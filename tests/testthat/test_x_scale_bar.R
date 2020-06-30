context("x.scale.bar")
library(ringdater)

test_that("x.scale.bar generates a series of numbers of be used as breaks in ggplot graphs", {

  expect_equal(length(x.scale.bar(x.min = 0,x.max = 500)), 26)
  expect_equal(length(x.scale.bar(x.min = 0,x.max = 50)), 11)

  expect_error(x.scale.bar(x.min = 100,x.max = -50))
  expect_error(x.scale.bar(x.min = "not_a_num",x.max = 100))
  expect_error(x.scale.bar(x.min = 100,x.max = "not_a_num"))
})
