context("running_lead_lag")
library(ringdater)

test_that("running_lead_lag produces a data.table of lrunning lead lag correlations for use in producing a heatmap", {
  undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
  undated_data <- load_undated(undated_path)
  undated_data <- name_check(undated_data)
  undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)

  expect_equal(ncol(running_lead_lag(the_data = undated,
                                       s1 = colnames(undated)[2],
                                       s2 = colnames(undated)[3],
                                       neg_lag = -20,
                                       pos_lag = 20,
                                       win = 21,
                                       complete = FALSE)), 3)

  expect_equal(nrow(running_lead_lag(the_data = undated,
                                       s1 = colnames(undated)[2],
                                       s2 = colnames(undated)[3],
                                       neg_lag = -20,
                                       pos_lag = 20,
                                       win = 21,
                                       complete = FALSE)), 1881)

  expect_error(running_lead_lag(the_data = c(1:50),
                                  s1 = colnames(undated)[2],
                                  s2 = colnames(undated)[3],
                                  neg_lag = -20,
                                  pos_lag = 20,
                                  win = 21,
                                  complete = FALSE))

  expect_error(running_lead_lag(the_data = undated,
                                     s1 = "not_a_sample",
                                     s2 = colnames(undated)[3],
                                     neg_lag = -20,
                                     pos_lag = 20,
                                     win = 21,
                                     complete = FALSE))

  expect_error(running_lead_lag(the_data = undated,
                                s1 = colnames(undated)[2],
                                s2 = "not_a_sample",
                                neg_lag = -20,
                                pos_lag = 20,
                                win = 21,
                                complete = FALSE))

  expect_error(running_lead_lag(the_data = undated,
                                s1 = colnames(undated)[2],
                                s2 = colnames(undated)[3],
                                neg_lag = 50,
                                pos_lag = 20,
                                win = 21,
                                complete = FALSE))

  expect_error(running_lead_lag(the_data = undated,
                                s1 = colnames(undated)[2],
                                s2 = colnames(undated)[3],
                                neg_lag = "not_a_num",
                                pos_lag = 20,
                                win = 21,
                                complete = FALSE))

  expect_error(running_lead_lag(the_data = undated,
                                s1 = colnames(undated)[2],
                                s2 = colnames(undated)[3],
                                neg_lag = -20,
                                pos_lag = "not_a_num",
                                win = 21,
                                complete = FALSE))
  # win too small
  expect_error(running_lead_lag(the_data = undated,
                                s1 = colnames(undated)[2],
                                s2 = colnames(undated)[3],
                                neg_lag = -20,
                                pos_lag = 20,
                                win = -1,
                                complete = FALSE))

  expect_error(running_lead_lag(the_data = undated,
                                s1 = colnames(undated)[2],
                                s2 = colnames(undated)[3],
                                neg_lag = -20,
                                pos_lag = 20,
                                win = 21,
                                complete = "not_a_bool"))


})
