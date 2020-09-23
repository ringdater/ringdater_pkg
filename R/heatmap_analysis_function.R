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
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' undated_data <- load_undated(undated_path)
#' undated_data <- name_check(undated_data)
#' undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)
#' heatmap_analysis(the_data = undated, s1 = colnames(undated)[2], s2 = colnames(undated)[3],
#'                  neg_lag = -20, pos_lag = 20, win = 21, complete = FALSE)

heatmap_analysis<-function(the_data, s1, s2, neg_lag = -20, pos_lag = 20, win = 21, center = 0, complete = TRUE, sel_col_pal = 1, font_size = 12, axis_line_width = 1, plot_line = 1, leg_size = 1){

  if(is.null(the_data) || class(the_data) != "data.frame"){
    stop("Warning. Error in running_lead_lag. the_data is not a valid data.frame")
  }
  if(!s1 %in% colnames(the_data)){
    stop("Error in running_lead_lag: s1 is not a valid sample ID")
  }
  if(!s2 %in% colnames(the_data)){
    stop("Error in running_lead_lag: s2 is not a valid sample ID")
  }
  if(class(pos_lag) != "numeric" || pos_lag < neg_lag || pos_lag%%1 !=0){
    stop("Error in running_lead_lag: pos_lag should be an numeric integer with a value greater than leg_lag")
  }
  if(class(neg_lag) != "numeric" || neg_lag > pos_lag || neg_lag%%1 !=0){
    stop("Error in running_lead_lag: neg_lag should be an numeric integer with a value less than pos_lag")
  }
  if(class(win) != "numeric" || win%%1 !=0 || win < 5){
    stop("Error in running_lead_lag: win should be a numeric integer")
  }
  if(class(center) != "numeric" || center%%1 !=0){
    stop("Error in running_lead_lag: center should be an numeric integer")
  }
  if (class(complete) !="logical"){
    stop("Error in running_lead_lag: complete should be TRUE or FALSE")
  }
  if(class(sel_col_pal) != "numeric" || !sel_col_pal %in% 1:4){
    stop("Error in running_lead_lag: sel_col_pal should be a numeric integer 1:4")
  }
  if(class(font_size) != "numeric" || font_size%%1 !=0 || font_size <0){
    stop("Error in running_lead_lag: sel_col_pal should be a numeric integer >0")
  }
  if(class(axis_line_width) != "numeric" ||  axis_line_width <0){
    stop("Error in running_lead_lag: axis_line_width should be a numeric value >0")
  }
  if(class(plot_line) != "numeric" ||  plot_line <0){
    stop("Error in running_lead_lag: plot_line should be a numeric value >0")
  }
  if(class(leg_size) != "numeric" ||  leg_size <0){
    stop("Error in running_lead_lag: leg_size should be a numeric value >0")
  }

  plot.data <-  running_lead_lag(the_data=the_data, s1=s1, s2=s2, neg_lag = neg_lag + center, pos_lag = pos_lag + center, win = win, complete = complete)

  the_plot <- plotting_sing_hm(plot.data = plot.data,
                               the_data=the_data,
                               s1=s1,
                               s2=s2,
                               neg_lag = neg_lag + center,
                               pos_lag = pos_lag + center,
                               leg_size = leg_size,
                               font_size = font_size,
                               axis_line_width = axis_line_width,
                               plot_line = plot_line)

  return(the_plot)
}
