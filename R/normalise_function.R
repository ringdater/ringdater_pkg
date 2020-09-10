#' Detrend a set of ring width series contained in a data.frame
#'
#' This function detrends data in a dataframe
#' @keywords detrending
#' @param the.data A dataframe containing the data to be detrended.
#' @param detrending_select A numeric integer to define which detrending method to use. 1 = Do nothing, 2 = Z-scores, 3 = spline detrending, 4 = Mod. negative exponentia, 5 = Friedman, 6 = ModHugershoff, 7 = First difference. Spline is slected by default.
#' @param splinewindow A numeric integer to define the length of the spline to be used in splne detrending (if option 3 selected).
#' @param ARmod set to true to apply prewhitening to series
#' @param logT set to true to apply log transformation to data
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' undated_data <- load_undated(undated_path)
#' normalise(the.data = undated_data,
#'           detrending_select = 3,
#'           splinewindow = 21,
#'           ARmod = TRUE,
#'           logT = TRUE)


normalise<-function(the.data, detrending_select = 1, splinewindow = 21, ARmod = FALSE, logT = FALSE){
  if (!detrending_select %in% c(1:7)){
    stop("Error in normalise(). detrending_select must be a numeric integer from 1 to 7.")

  }
  if (class(splinewindow) != "numeric"){
    stop("Error in normalise(). splinewindow must be a numeric integer")

  }
  if (splinewindow <5 || splinewindow >200){
    stop("Error in normalise(). splinewindow must be a numeric integer from 5 to 500.")

  }
  if (class(the.data) != "data.frame"){
    stop("Error in normalise(). Required data are not a data.frame")

  }
  if (ncol(the.data)<2){
    stop("Error in normalise(). Insufficient data to calculate correlations")

  }
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

      if(ARmod){
        A <- whitenSeries(A)
      }
      if(logT){
        #Add small constant to avoid taking log of negative number
        A <- A + (abs(min(A, na.rm = T)) + 1)*7/6
        #Log transform each individual value
        A <- log(A)
      }

      det_tmp<-comb.NA(det_tmp, A, fill = NA)
      series_a<-series_a+1
    }

    series_length = nrow(new)

    det_tmp<-subset(det_tmp, !is.na(det_tmp[,1]))

    colnames(det_tmp)<-series_IDs

    return(det_tmp)

}
