context("line_plot")
library(ringdater)

test_that("line_plot produce a line plot of two timeseries withthe option to lag one of the records", {


 test1 <- system.file("extdata", "undated_example.csv", package="ringdater")
 the_data <- load_undated(test1)
 series_1 <- colnames(the_data)[2]
 series_2 <- colnames(the_data)[3]
 graph <- line_plot(the_data = the_data, series_1_nm = series_1, series_2_nm = series_2, lag = -7)

 expect_equal(class(graph)[1], "gg")

})
