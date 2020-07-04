#' Theme for plots in RingdateR
#'
#' Modify the theme used in the plots displayed in RingdateR
#' @keywords GUI
#' @param text.size A numeric integer to define the size of the text displayed in the figures
#' @param line.width A numeric integer to set the width of the axis lines.
#' @param leg_size A numeric integer to set the size of the colour legends in the heat map plots.
#' @param l A numeric integer to define the size of the left margin
#' @import ggplot2
#' @export
#' @examples
#' # plot_data <-data.frame(x = 1:10, y = 1:10)
#' # ggplot(data = plot_data, aes(x = x, y=y))+
#' # geom_point() + R_dateR_theme()
#'

R_dateR_theme<-function(text.size = 12, line.width = 1, l = 10, leg_size = 3){
  if (class(text.size)!="numeric" || text.size <= 0){
    stop("Warning: an error occured in R_dateR_theme: text.size was not a numeric value >0")
  }
  if (class(line.width)!="numeric" || line.width <= 0){
    stop("Warning: an error occured in R_dateR_theme: line.width was not a numeric value >0")
  }
  if (class(leg_size)!="numeric" || leg_size <= 0){
    stop("Warning: an error occured in R_dateR_theme: leg_size was not a numeric value >0")
  }
  if (class(l)!="numeric"){
    stop("Warning: an error occured in R_dateR_theme: l (left margin) was not a numeric value >0")
  }

  theme(
    text = element_text(size = text.size),
    panel.background = element_blank(),

    axis.line = element_line(size = line.width,
                             colour = "black"),
    axis.ticks = element_line(colour = "black",
                              size = line.width),
    axis.text = element_text(size = text.size,
                             color = "black"),
    axis.ticks.length = unit(.25, "cm"),
    plot.margin = margin(10, 0, 0, l), #T,R,B,L
    axis.title.y = element_text(size = text.size,
                                margin=margin(t= 0, r = 20, b =10, l = )),
    legend.key = element_rect(size = 10),
    panel.grid.major = element_line(colour = "grey",
                                    size = 0.5,
                                    linetype = "dashed"),
    legend.position="bottom",
    legend.text = element_text(size = text.size),
    legend.key.width = unit(leg_size,"cm")
  )
}
