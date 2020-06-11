#' Generate the data for plotting detrending curves
#'
#' This function creates a dataframe containing teh detrending curve data used in the normalise function.
#' @keywords detrending curves
#' @param series_data A data.frame containing the undetrended individual ring width series.
#' @param detrending_select A numeric integer to define which detrending method to use. 1 = Do nothing, 2 = Z-scores, 3 = spline detrending, 4 = Mod. negative exponentia, 5 = Friedman, 6 = ModHugershoff, 7 = First difference. Spline is slected by default.
#' @param splinewindow A numeric integer to define the length of the spline to be used in splne detrending (if option 3 selected).
#' @importFrom dplR detrend.series
#' @export
#' @examples
#' path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' the_data<-load_undated(path)
#' detcurves(series_data = the_data, detrending_select = 3, splinewindow = 21)


detcurves<-function(series_data, detrending_select = 1, splinewindow = 21){

  if (detrending_select %in% c(1:7) == FALSE){
    warning("Error in detcurves(). detrending_select must be a numeric integer from 1 to 7. Set to default spine detrending.")
    detrending_select <- 3
  }

  if (splinewindow %in% c(5:500) == FALSE){
    warning("Error in detcurves(). splinewindow must be a numeric integer from 5 to 500. Set to default spine length = 21.")
    splinewindow <- 21
  }

  # example check for a dataframe with at least 2 columns of data.
  if (class(series_data) != "data.frame"){
    warning("Error in detcurves(). Required data are not a data.frame")
    return(NULL)
    # Check thre is enough data in the data.frame
  } else if (ncol(series_data)<2){
    warning("Error in detcurves(). Insufficient data to calculate correlations")
    return(NULL)
  } else {

    no.series<-ncol(series_data)-1
    series_IDs<-c(colnames(series_data))
    series_a<-2 # this is two as the first set of series data should be in the second column
    new<-data.frame(series_data[,1], stringsAsFactors = T) # sets up the data frame to put the detrended data into
    tmp_diff<-c()

    for(i in 1:no.series){ # this runs the detrending on all the series data
      if(detrending_select==1){
        A<- series_data[,series_a]
      } else if (detrending_select==2){
        A<- series_data[,series_a]
      } else if (detrending_select==3){
        A<- detrend.series(series_data[,series_a], method = "Spline", nyrs= splinewindow, make.plot = FALSE, return.info = TRUE)
        A <- A$curves
      } else if (detrending_select==4){
        A<- detrend.series(series_data[,series_a], method = "ModNegExp", make.plot = FALSE, return.info = TRUE)
        A <- A$curves
      } else if (detrending_select==5){
        A<- detrend.series(series_data[,series_a], method = "Friedman", make.plot = FALSE, pos.slope =TRUE, return.info = TRUE)
        A <- A$curves
      } else if (detrending_select==6){
        A<- detrend.series(series_data[,series_a], method = "ModHugershoff", make.plot = FALSE, pos.slope =TRUE, return.info = TRUE)
        A <- A$curves
      }  else if (detrending_select==7){
        A<- series_data[,series_a]
      }

      new<-cbind(new, A)
      series_a<-series_a+1
    }
    series_length = nrow(new)

    colnames(new)<-series_IDs

    return(new)
  }
}
