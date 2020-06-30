context("filtered_data")
library(ringdater)

test_that("filtered_data sorts the results ofthe lead-lag analyses for the large results table", {
# For speed, the results of the analyses above are imported as two CSV files and put
 # together into a list (chron_comp).
 exmp_analysis_1 <- system.file("extdata", "chron_comp_1.csv", package="ringdater")
 chron_comp_1 <- read.csv(exmp_analysis_1, stringsAsFactors = FALSE, header = TRUE)

 exmp_analysis_2 <- system.file("extdata", "chron_comp_2.csv", package="ringdater")
 chron_comp_2 <- read.csv(exmp_analysis_2, stringsAsFactors = FALSE, header = TRUE)

 chron_comp<-list(chron_comp_1,chron_comp_2)

 filtered_data <- filter_crossdates(the_data = as.data.frame(chron_comp[1]),
                                   r_val = 0.4,
                                   p_val = 0.05,
                                   overlap = 30,
                                   target = "mean_chronology")

 expect_equal(ncol(filtered_data), 17)
 expect_equal(nrow(filtered_data), 10)
 expect_equal(class(filtered_data), "data.frame")
})


