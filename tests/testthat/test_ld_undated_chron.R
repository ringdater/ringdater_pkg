context("ld_undated_chron")
library(ringdater)

test_that("ld_undated_chron loads an undated chronology and automatically detrends it to calculate an arithmetic mean chronology", {

  chron_path  <- system.file("extdata", "undated_chron.xlsx", package="ringdater")
  chron_data  <- ld_undated_chron(chron_path)
  expect_equal(ncol(chron_data), 2)
  expect_equal(nrow(chron_data), 329)
  expect_equal(class(chron_data), "data.frame")
})
