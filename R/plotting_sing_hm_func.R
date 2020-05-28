#' Plot running lead_lag analysis
#'
#' This function plots the running lead_lag analysis
#' @keywords pairwise_lead_lag
#' @param plot.data a three column data frame containing the results from the running_lead_lag function
#' @param the_data A dataframe containing the timeseries. First column should contain dates.
#' @param s1 the name of a sample - must match a column name exactly
#' @param s2 the name of a sample - must match a column name exactly
#' @param neg_lag An integer
#' @param pos_lag An integer
#' @param font_size The size of the font (numeric)
#' @param axis_line_width The width of the axis (numeric)
#' @param plot_line The width of the line (numeric)
#' @param sel_col_pal The colour pallate to use (numeric)
#' @param leg_size a numeric value to define the length of the colour scale legend
#' @export

plotting_sing_hm<- function(plot.data , the_data, s1, s2, font_size = 12, axis_line_width = 0.5, plot_line = 0.5, neg_lag = -20, pos_lag = 20, sel_col_pal = 1, leg_size = 1){
  if(is.null(plot.data)){return(warning("Insufficient overlap to perform running correlation analysis"))
  } else {

    new<-the_data

    series.names<-colnames(new)

    plot.title<- paste0(s1, " vs ", s2)
    y.lab<- paste0("lag (years from ", s1, ")")

    x.lab<-"Year"

    col_scale <- col_pal(sel_col_pal)

    # Plot a heat map of the correlations

    plot2<-ggplot(plot.data, aes(x=plot.data[,1], y=plot.data[,2]), na.rm=TRUE) + geom_raster(aes(fill = plot.data[,3])) +
      R_dateR_theme(text.size = as.numeric(font_size), line.width = as.numeric(axis_line_width), leg_size = leg_size) +
      scale_fill_gradientn(colours = col_scale, limits = c(-1,1)) +labs(fill = "Correl. (R)", x = x.lab, y= y.lab, title=plot.title) +
      scale_x_continuous(breaks = x.scale.bar(round(min(plot.data[,1]),-1), round(max(plot.data[,1]),-1))) +
      scale_y_continuous(breaks = y.scale.bar(min(plot.data[,2]),max(plot.data[,2])))

    return(plot2)
  }
}
