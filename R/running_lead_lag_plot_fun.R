#' running lead_lag analysis heat maps
#'
#' This function rperforms the full running lead-lag analysis and produces a plot of the results
#' @keywords pairwise_lead_lag
#' @param the_data A dataframe containing the timeseries. First column should contain dates.
#' @param s1 the name of a sample as a character string - must match a column name exactly
#' @param s2 the name of a sample  as a character string - must match a column name exactly
#' @param neg_lag An integer
#' @param pos_lag An integer
#' @param complete A boolean to classify whther to run the lead-lag over the maximum range of leads and lags
#' @param win The window with which the running correlations are calculated over
#' @param font_size The size of the font (numeric)
#' @param axis_line_width The width of the axis (numeric)
#' @param plot_line The width of the line (numeric)
#' @param sel_col_pal The colour pallate to use (numeric)
#' @param center A numeric integer used to center the lead lag limits around. e.g. the pos lag is added to this value.
#' @param leg_size A numeric integer to define the size of the colour bar legend.
#' @export

  heatmap_analysis<-function(the_data, s1, s2, neg_lag = -20, pos_lag = 20, win = 21, center = 0, complete = TRUE, sel_col_pal = 1, font_size = 12, axis_line_width = 1, plot_line = 1, leg_size = 1){

  plot.data <-  running_lead_lag(the_data=the_data, s1=s1, s2=s2, neg_lag = neg_lag + center, pos_lag = pos_lag + center, win = win, complete = complete)

  the_plot <- plotting_sing_hm(plot.data = plot.data, the_data=the_data, s1=s1, s2=s2, neg_lag = neg_lag + center, pos_lag = pos_lag + center, leg_size = leg_size)

  return(the_plot)
}
