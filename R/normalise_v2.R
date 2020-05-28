#' Detrend a set of ring width series contained in a data.frame
#'
#' This function detrends data in a dataframe
#' @keywords detrending
#' @param the.data A dataframe containingthe data to be detrended.
#' @param detrending_select A numeric integer to define which detrending method to use. 1 = Do nothing, 2 = Z-scores, 3 = spline detrending, 4 = Mod. negative exponentia, 5 = Friedman, 6 = ModHugershoff, 7 = First difference. Spline is slected by default.
#' @param splinewindow A numeric integer to define the length of the spline to be used in splne detrending (if option 3 selected).
#' @export

normalise<-function(the.data, detrending_select = 1, splinewindow = 21){
  if (detrending_select %in% c(1:7) == FALSE){
    warning("Error in normalise(). detrending_select must be a numeric integer from 1 to 7. Set to default spine detrending.")
    detrending_select <- 3
  }

  if (splinewindow %in% c(5:500) == FALSE){
    warning("Error in normalise(). splinewindow must be a numeric integer from 5 to 500. Set to default spine length = 21.")
    splinewindow <- 21
  }

  # example check for a dataframe with at least 2 columns of data.
  if (class(the.data) != "data.frame"){
    warning("Error in normalise(). Required data are not a data.frame")
    return(NULL)
    # Check thre is enough data in the data.frame
  } else if (ncol(the.data)<2){
    warning("Error in normalise(). Insufficient data to calculate correlations")
    return(NULL)
  } else {
    series_data<-the.data
    no.series<-ncol(the.data)-1
    series_IDs<-c(colnames(the.data))
    series_a<-2 # this is two as the first set of series data should be in the second column
    new<-data.frame(the.data[,1], stringsAsFactors = T) # sets up the data frame to put the detrended data into

    series_a<-2
    tmp_diff<-c()
    det_tmp<-data.frame(the.data[,1])

    for(i in 1:no.series){ # this runs the detrending on all the series data
      if(detrending_select==1){
        A<- series_data[,series_a]
      } else if (detrending_select==2){
        A<- scale(series_data[,series_a], center = T, scale = T)
      } else if (detrending_select==3){
        A<- detrend.series(series_data[,series_a], method = "Spline", nyrs= splinewindow, make.plot = FALSE, pos.slope =TRUE)
      } else if (detrending_select==4){
        A<- detrend.series(series_data[,series_a], method = "ModNegExp", make.plot = FALSE, pos.slope =TRUE)
      } else if (detrending_select==5){
        A<- detrend.series(series_data[,series_a], method = "Friedman", make.plot = FALSE, pos.slope =TRUE)
      } else if (detrending_select==6){
        A<- detrend.series(series_data[,series_a], method = "ModHugershoff", make.plot = FALSE, pos.slope =TRUE)
      } else if (detrending_select==7){
        A<- series_data[,series_a]
        for (j in 1: length(A)){
          tmp<-A[j+1]-A[j]
          tmp_diff<-c(tmp_diff,tmp)
        }
        A<-tmp_diff
        tmp_diff<-c()
      }


      det_tmp<-comb.NA(det_tmp, A, fill = NA)
      series_a<-series_a+1
    }

    series_length = nrow(new)

    det_tmp<-subset(det_tmp, !is.na(det_tmp[,1]))

    colnames(det_tmp)<-series_IDs

    return(det_tmp)
  }
}
