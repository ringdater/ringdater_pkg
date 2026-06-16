#' Theme for plots in RingdateR
#'
#' Modify the theme used in the plots displayed in RingdateR
#' @keywords GUI
#' @param text_size A numeric integer to define the size of the text displayed in the figures
#' @param line_width A numeric integer to set the width of the axis lines.
#' @param leg_size A numeric integer to set the size of the colour legends in the heat map plots.
#' @param l A numeric integer to define the size of the left margin
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' plot_data <- data.frame(x = 1:10, y = 1:10)
#' ggplot(data = plot_data, aes(x = x, y = y)) +
#' geom_point() + R_dateR_theme()
#'

R_dateR_theme <- function(text_size = 12, line_width = 1, l = 10, leg_size = 3) {
  if (!is.numeric(text_size) || text_size <= 0) {
    stop("Warning: an error occurred in R_dateR_theme: text_size was not a numeric value > 0")
  }
  if (!is.numeric(line_width) || line_width <= 0) {
    stop("Warning: an error occurred in R_dateR_theme: line_width was not a numeric value > 0")
  }
  if (!is.numeric(leg_size) || leg_size <= 0) {
    stop("Warning: an error occurred in R_dateR_theme: leg_size was not a numeric value > 0")
  }
  if (!is.numeric(l)) {
    stop("Warning: an error occurred in R_dateR_theme: l (left margin) was not a numeric value")
  }

  theme(
    text = element_text(size = text_size),
    panel.background = element_blank(),
    axis.line = element_line(linewidth = line_width, colour = "black"),
    axis.ticks = element_line(colour = "black", linewidth = line_width),
    axis.text = element_text(size = text_size, colour = "black"),
    axis.ticks.length = unit(.25, "cm"),
    plot.margin = margin(10, 0, 0, l), # T, R, B, L
    axis.title.y = element_text(size = text_size, margin = margin(t = 0, r = 20, b = 10, l = l)),
    legend.key = element_rect(linewidth = 1),
    panel.grid.major = element_line(colour = "grey", linewidth = 0.5, linetype = "dashed"),
    legend.position = "bottom",
    legend.text = element_text(size = text_size),
    legend.key.width = unit(leg_size, "cm")
  )
}

