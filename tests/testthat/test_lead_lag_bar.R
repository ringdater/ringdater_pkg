context("lead_lag_bar")
library(ringdater)

test_that("lead_lag_bar produces a bar chart of the lead lag results", {

exmp_analysis_1 <- system.file("extdata", "chron_comp_1.csv", package="ringdater")
 chron_comp_1 <- read.csv(exmp_analysis_1, stringsAsFactors = FALSE, header = TRUE)

 exmp_analysis_2 <- system.file("extdata", "chron_comp_2.csv", package="ringdater")
 chron_comp_2 <- read.csv(exmp_analysis_2, stringsAsFactors = FALSE, header = TRUE)

 chron_comp<-list(chron_comp_1,chron_comp_2)

 graph <- lead_lag_bar(the_data = as.data.frame(chron_comp[2]),
              sample_1 = "mean_chronology",
              sample_2 = "sample_a")

 expect_equal(class(graph)[1], "gg")

})
