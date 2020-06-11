#' Generate a plot of the dtrended data
#'
#' This function produces a plot of the raw and detrended ring width data as well as the corresponding autocorrelation.
#' @keywords pos, image pro
#' @export
#' @param undet.data A data.frame containing at least two columns. First column years/ring number second column ring widths. Can be morethan two columns of ring width data.
#' @param first_series A column name
#' @param detrending_select A numeric integer to define which detrending method to use. 1 = Do nothing, 2 = Z-scores, 3 = spline detrending, 4 = Mod. negative exponentia, 5 = Friedman, 6 = ModHugershoff, 7 = First difference. Spline is slected by default.
#' @param splinewindow A numeric integer to define the length of the spline to be used in splne detrending (if option 3 selected).
#' @param font_size A numeric integer to define the size of axis labels
#' @param axis_line_width A numeric integer to define the thickness ofthe axis lines
#' @param plot_line A numeric integer to define the thickness of the plotted lines
#' @examples
#' path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' the_data <- load_undated(path)
#' series <- colnames(the_data)[2]
#' detrending.plot.fun(undet.data = the_data,
#'                    first_series = series,
#'                    detrending_select = 3,
#'                    splinewindow = 21)

detrending.plot.fun<- function(undet.data, first_series, detrending_select = 3, splinewindow = 21, font_size = 12, axis_line_width = 1, plot_line = 1){

  # Get undetrended data
  un.det.years       <-undet.data[,1]
  undet.data         <-undet.data[[first_series]]
  undet.data         <-comb.NA(un.det.years,undet.data, fill = NA)
  undet.data         <-subset(undet.data,complete.cases(undet.data))

  det_nd<- normalise(undet.data, detrending_select = detrending_select, splinewindow = splinewindow)

  curve<- detcurves(series_data = undet.data, detrending_select = detrending_select, splinewindow = splinewindow)

  raw_auto <-auto_correl(undet.data)
  det_aut<- auto_correl(det_nd)

  x.lab<-"Increment number"

  plot1<- ggplot()+
    geom_line(data = undet.data, aes(x = undet.data[,1], y = undet.data[,2]), na.rm=TRUE, alpha =0.75, size = plot_line) +
    geom_line(data = curve, aes(x = curve[,1], y = curve[,2]), na.rm=TRUE, size = plot_line+0.5)+
    R_dateR_theme(text.size = font_size, line.width = axis_line_width,l=20) +
    labs(title = paste0(first_series, " raw data. Thick black line = the detrneding curve applied")) + ylab("Increment width") + xlab(x.lab) +
    scale_x_continuous(breaks = x.scale.bar(min(as.numeric(undet.data[,1])), max(as.numeric(undet.data[,1]))))

  plot2<- ggplot()+
    geom_line(data = det_nd, aes(x = det_nd[,1], y = det_nd[,2]), na.rm=TRUE, alpha =1, size = plot_line, colour = "red") +
    R_dateR_theme(text.size = font_size, line.width = axis_line_width,l=20) +
    labs(title = "Detrended data") + ylab("Increment width") + xlab(x.lab) +
    scale_x_continuous(breaks = x.scale.bar(min(as.numeric(undet.data[,1])), max(as.numeric(undet.data[,1]))))

  plot3 <- ggplot()+
    geom_line(data = raw_auto, aes(x = raw_auto[,1], y = raw_auto[,2]), na.rm=TRUE, size = plot_line) +
    geom_line(data = det_aut, aes(x = det_aut[,1], y = det_aut[,2]), na.rm=TRUE, size = plot_line, colour = "red") +
    R_dateR_theme(text.size = font_size, line.width = axis_line_width,l=20) +xlab("lag (Year)") + ylab("Correl. (R)") +
    labs(title="Black line = raw data auto correlation; Red line = detrended data autocorrelation") +
    scale_x_continuous(breaks = c(0:10))

  g1 <- ggplotGrob(plot1)
  g2<-  ggplotGrob(plot2)
  g3<-  ggplotGrob(plot3)

  g<-rbind(g1, g2, g3, size = "first")
  both <- grid.draw(g)
 }
