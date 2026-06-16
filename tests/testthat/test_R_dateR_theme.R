context("R_dateR_theme")
library(ringdater)
library(ggplot2)

test_that("R_dateR_theme modifies the ggplot theme", {
  plot_data <-data.frame(x = 1:10, y = 1:10)

  expect_equal(class(ggplot(data = plot_data, aes(x = x, y=y))+
                       geom_point() +
                       R_dateR_theme(text_size = 12,
                                     line_width = 1,
                                     l = 10,
                                     leg_size = 1))[1], "ggplot2::ggplot")

  expect_error(ggplot(data = plot_data, aes(x = x, y=y))+
                       geom_point() +
                       R_dateR_theme(text_size = "not a number",
                                     line_width = 1,
                                     l = 10,
                                     leg_size = 1))

  expect_error(ggplot(data = plot_data, aes(x = x, y=y))+
                   geom_point() +
                   R_dateR_theme(text_size = 12,
                                 line_width = "not a number",
                                 l = 10,
                                 leg_size = 1))

  expect_error(ggplot(data = plot_data, aes(x = x, y=y))+
                   geom_point() +
                   R_dateR_theme(text_size = 12,
                                 line_width = 1,
                                 l = "not a number",
                                 leg_size = 1))

  expect_error(ggplot(data = plot_data, aes(x = x, y=y))+
                   geom_point() +
                   R_dateR_theme(text_size = 12,
                                 line_width = 1,
                                 l = 10,
                                 leg_size = "not a number"))


})
