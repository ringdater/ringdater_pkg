context("align_to_chron")
library(ringdater)

test_that("align_to_chron aligns crossdated series with individual samples contained in the chronology", {
  undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
  undated_data <- load_undated(undated_path)
  undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)

  chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
  chron_data  <- load_chron(chron_path)
  chrono_det  <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)
  chrono      <- data.frame(chrono_det[,1], rowMeans(chrono_det[,-1], na.rm = TRUE))
  colnames(chrono)<-c("year", "mean_chronology")

  chron_n_series <- comb.NA(chrono, undated[,-1], fill = NA)

  exmp_analysis_1 <- system.file("extdata", "chron_comp_1.csv", package="ringdater")
  chron_comp_1 <- read.csv(exmp_analysis_1, stringsAsFactors = FALSE, header = TRUE)

  exmp_analysis_2 <- system.file("extdata", "chron_comp_2.csv", package="ringdater")
  chron_comp_2 <- read.csv(exmp_analysis_2, stringsAsFactors = FALSE, header = TRUE)

  chron_comp <- list(chron_comp_1,chron_comp_2)

  filtered_data <- filter_crossdates(the_data = as.data.frame(chron_comp[1]),
                                     r_val = 0.4,
                                     p_val = 0.05,
                                     overlap = 30,
                                     target = "mean_chronology")

  aligned_data <- align_series(the_data = chron_n_series, cross_dates = filtered_data, sel_target = "mean_chronology")

  fully_aligned <- align_to_chron(the.data = aligned_data, chrono = chrono_det)

  expect_equal(ncol(fully_aligned), 24)
  expect_equal(nrow(fully_aligned), 329)
  expect_equal(class(fully_aligned), "data.frame")

})
