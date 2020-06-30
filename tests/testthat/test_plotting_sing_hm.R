context("plotting_sing_hm")
library(ringdater)

test_that("plotting_sing_hm plots a running lead lag correaltion heat map", {
  chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
  chron_data  <- load_chron(chron_path)
  chrono_det  <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)

  plot_data <- running_lead_lag(the_data = chrono_det,
                               s1 = colnames(chrono_det)[2],
                               s2 = colnames(chrono_det)[3],
                               neg_lag = -20,
                               pos_lag = 10,
                               win = 21,
                               complete = FALSE)

  class(plotting_sing_hm(plot.data = plot_data,
                   the_data = chrono_det,
                   s1 = colnames(chrono_det)[2],
                   s2 = colnames(chrono_det)[3],
                   font_size = 12,
                   axis_line_width = 0.5,
                   plot_line = 0.5,
                   neg_lag = -20,
                   pos_lag = 20,
                   sel_col_pal = 1,
                   leg_size = 1))

  expect_equal(class(plotting_sing_hm(plot.data = plot_data,
                                              the_data = chrono_det,
                                              s1 = colnames(chrono_det)[2],
                                              s2 = colnames(chrono_det)[3],
                                              font_size = 12,
                                              axis_line_width = 0.5,
                                              plot_line = 0.5,
                                              neg_lag = -20,
                                              pos_lag = 20,
                                              sel_col_pal = 1,
                                              leg_size = 1))[1], "gg")
  # Null data
  expect_error(plotting_sing_hm(plot.data = NULL,
                                the_data = chrono_det,
                                s1 = colnames(chrono_det)[2],
                                s2 = colnames(chrono_det)[3],
                                font_size = 12,
                                axis_line_width = 0.5,
                                plot_line = 0.5,
                                neg_lag = -20,
                                pos_lag = 20,
                                sel_col_pal = 1,
                                leg_size = 1))

  # the_data not a data.frame
  expect_error(plotting_sing_hm(plot.data = plot_data,
                                the_data = c(1:50),
                                s1 = colnames(chrono_det)[2],
                                s2 = colnames(chrono_det)[3],
                                font_size = 12,
                                axis_line_width = 0.5,
                                plot_line = 0.5,
                                neg_lag = -20,
                                pos_lag = 20,
                                sel_col_pal = 1,
                                leg_size = 1))
  # invalid sample ID1
  expect_error(plotting_sing_hm(plot.data = plot_data,
                                the_data = chrono_det,
                                s1 = "This is not a sample",
                                s2 = colnames(chrono_det)[3],
                                font_size = 12,
                                axis_line_width = 0.5,
                                plot_line = 0.5,
                                neg_lag = -20,
                                pos_lag = 20,
                                sel_col_pal = 1,
                                leg_size = 1))

  expect_error(plotting_sing_hm(plot.data = plot_data,
                                the_data = chrono_det,
                                s1 = colnames(chrono_det)[2],
                                s2 = "This is not a sample 2",
                                font_size = 12,
                                axis_line_width = 0.5,
                                plot_line = 0.5,
                                neg_lag = -20,
                                pos_lag = 20,
                                sel_col_pal = 1,
                                leg_size = 1))
  # invalid text size class
  expect_error(plotting_sing_hm(plot.data = plot_data,
                                the_data = chrono_det,
                                s1 = colnames(chrono_det)[2],
                                s2 = colnames(chrono_det)[3],
                                font_size = "not a number",
                                axis_line_width = 0.5,
                                plot_line = 0.5,
                                neg_lag = -20,
                                pos_lag = 20,
                                sel_col_pal = 1,
                                leg_size = 1))
  # error with line width
  expect_error(plotting_sing_hm(plot.data = plot_data,
                                the_data = chrono_det,
                                s1 = colnames(chrono_det)[2],
                                s2 = colnames(chrono_det)[3],
                                font_size = 12,
                                axis_line_width = -1,
                                plot_line = 0.5,
                                neg_lag = -20,
                                pos_lag = 20,
                                sel_col_pal = 1,
                                leg_size = 1))

  # neg lag is bigger than pos lag
  expect_error(plotting_sing_hm(plot.data = plot_data,
                                the_data = chrono_det,
                                s1 = colnames(chrono_det)[2],
                                s2 = colnames(chrono_det)[3],
                                font_size = 12,
                                axis_line_width = 0.5,
                                plot_line = 0.5,
                                neg_lag = 50,
                                pos_lag = 20,
                                sel_col_pal = 1,
                                leg_size = 1))
  #leg size invalid
  expect_error(plotting_sing_hm(plot.data = plot_data,
                                the_data = chrono_det,
                                s1 = colnames(chrono_det)[2],
                                s2 = colnames(chrono_det)[3],
                                font_size = 12,
                                axis_line_width = 0.5,
                                plot_line = 0.5,
                                neg_lag = -20,
                                pos_lag = 20,
                                sel_col_pal = 1,
                                leg_size = "one"))
  # col_pal too big
  expect_error(plotting_sing_hm(plot.data = plot_data,
                                the_data = chrono_det,
                                s1 = colnames(chrono_det)[2],
                                s2 = colnames(chrono_det)[3],
                                font_size = 12,
                                axis_line_width = 0.5,
                                plot_line = 0.5,
                                neg_lag = -20,
                                pos_lag = 20,
                                sel_col_pal = 7,
                                leg_size = 1))

})
