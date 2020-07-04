context("lead_lag_analysis")
library(ringdater)

test_that("lead_lag_analysis performs lead lag analyses between multiple timeseries", {
 undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
 undated_data <- load_undated(undated_path)
 undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)

 chron_path <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
 chron_data <- load_chron(chron_path)
 chrono      <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)
 chrono      <- data.frame(chrono[,1], rowMeans(chrono[,-1], na.rm = TRUE))
 colnames(chrono)<-c("year", "mean_chronology")

 chron_n_series <- comb.NA(chrono, undated[,-1], fill = NA)

 chron_comp <- lead_lag_analysis(the_data = chron_n_series,
                                mode = 2,
                                pos_lag= 20,
                                neg_lag = -20,
                                complete = FALSE,
                                shiny = FALSE)

  expect_equal(length(chron_comp), 2)
  expect_equal(class(chron_comp), "list")
})
