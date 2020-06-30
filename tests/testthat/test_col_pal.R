context("col_pal")
library(ringdater)

test_that("col_pal() produces a a list of colours to be used in the heatmap plots", {
  expect_equal(length(col_pal(colour_scale = 1)),3)
  expect_equal(length(col_pal(colour_scale = 2)),3)
  expect_equal(length(col_pal(colour_scale = 3)),5)
  expect_equal(length(col_pal(colour_scale = 4)),3)
})
