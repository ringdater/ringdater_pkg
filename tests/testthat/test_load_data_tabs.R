context("oad_data_tabs")
library(ringdater)

test_that("load_data_tabs produces a summary table of the loaded data", {

  undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
  undated_data <- load_undated(undated_path)
  test_data <- load_data_tabs(undated_data)

  expect_equal(ncol(test_data), 6)
  expect_equal(nrow(test_data), 13)
  expect_equal(class(test_data), "data.frame")
})
