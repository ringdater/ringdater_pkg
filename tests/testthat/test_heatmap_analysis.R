context("heatmap_analysis")
library(ringdater)

test_that("heatmap_analysis generates a running correlation heatmap", {

 undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
 undated_data <- load_undated(undated_path)
 undated_data <- name_check(undated_data)
 undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)
 graph <- heatmap_analysis(the_data = undated, s1 = colnames(undated)[2], s2 = colnames(undated)[3],
                  neg_lag = -20, pos_lag = 20, win = 21, complete = FALSE)

 expect_equal(class(graph)[1], "gg")
})
