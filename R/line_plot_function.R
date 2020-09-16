#' Line plot of two samples adjusted for lag
#'
#' PLot two timeseries and adjust the lag of the second series
#' @keywords lead-lag analysis
#' @param the_data A data.frame containing the series to be plotted. The first column should contain years/ring numbers
#' @param series_1_nm A character string containing a column name in the the_data to be plotted.
#' @param series_2_nm A character string containing a column name in the the_data to be plotted. This series will be lagged.
#' @param lag A numeric integer to lag series series_2_nm by.
#' @param text.size A numeric to set the axis font size
#' @param line.width A numeric to set the width of the line
#' @export
#' @examples
#' test1 <- system.file("extdata", "undated_example.csv", package="ringdater")
#' the_data <- load_undated(test1)
#' series_1 <- colnames(the_data)[2]
#' series_2 <- colnames(the_data)[3]
#' line_plot(the_data = the_data, series_1_nm = series_1, series_2_nm = series_2, lag = -7)

line_plot<-function(the_data, series_1_nm, series_2_nm, lag = 0, text.size = 12, line.width = 1){

  # example check for a dataframe with at least 2 columns of data.
  if (class(the_data) != "data.frame"){
    stop("Error in line_plot(). Required data are not a data.frame")
  }
  if (ncol(the_data)<=2){
    stop("Error in line_plot(). Insufficient data to calculate correlations")
  }
  if(class(series_1_nm) != "character"){
    stop("Error in line_plot(). series_1_nm is not a character string.")
  }
  if(series_1_nm %in% colnames(the_data) == FALSE){
    stop("Error in line_plot(). series_1_nm can not be found in the loaded data.")
  }
  if(class(series_2_nm) != "character"){
    stop("Error in line_plot(). series_2_nm is not a character string.")
  }
  if(series_2_nm %in% colnames(the_data) == FALSE){
    stop("Error in line_plot(). series_2_nm can not be found in the loaded data.")
  }
  if (class(lag) != "numeric" || lag %% 1 != 0){
    stop("Error in line_plot(). lag should be a numeric integer.")
  }

    series_1<-data.frame(the_data[,1], the_data[[series_1_nm]])
    series_2<-data.frame(the_data[,1], the_data[[series_2_nm]])

    series_1<-subset(series_1, (complete.cases(series_1)))
    series_2<-subset(series_2, (complete.cases(series_2)))

    series_2[,1]<-series_2[,1]+lag

    plot.title<-paste0(series_1_nm, " (black line) and ", series_2_nm, " lagged by ", lag, " years (red line)" )

    x.lab<- "Years"

    plot1<-ggplot()+
      geom_line(data = series_1, aes(x=series_1[,1], y=series_1[,2]), na.rm=TRUE, colour="black", size = 0.5) + labs(title = plot.title) +
      geom_line(data = series_2, aes(x=series_2[,1], y=series_2[,2]), na.rm=TRUE, colour="red", size = 0.5) + R_dateR_theme(text.size = text.size, line.width = line.width) +
      ylab("Standardised increment width") + xlab(x.lab) +
      scale_x_continuous(breaks = x.scale.bar(round(min(min(series_1[,1]),min(series_2[,1])), -1), round(max(max(series_1[,1]),max(series_2[,1])), -1)))

   return(plot1)

}
