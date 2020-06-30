context("dated_line_plot")
library(ringdater)

test_that("dated_line_plot produces data to plot the position of samples within a chronology", {
  chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
  chron_data  <- load_chron(chron_path)
  expect_equal(ncol(dated_line_plot(chron_data)), 3)
  expect_equal(nrow(dated_line_plot(chron_data)), 26)
  expect_equal(class(dated_line_plot(chron_data)), "data.frame")
})
