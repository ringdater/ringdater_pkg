#' prewhiten data
#'
#' This function prewhitens series using COFECHA methodology
#' @keywords RingdateR
#' @param series1 The file names, only used in the shiny server function.
#' @importFrom stats ar
#' @importFrom stats na.exclude

whitenSeries <- function(series1){

  if(length(series1) == 0){stop()
    } else {

    earlyNAs <- NULL
    lateNAs <- NULL
    #Count NA padding
    if (sum(is.na(series1)) > 0){
      if (is.na(series1)[1]){
        firstVal <- min(which(!is.na(series1)))
        earlyNAs <- firstVal - 1
      }else{
        earlyNAs <- 0
      }
      if (is.na(series1)[length(series1)]){
        lastVal <- max(which(!is.na(series1)))
        lateNAs <- length(series1) - lastVal
      }else{
        lateNAs <- 0
      }
    }

    #AR modelling
    seriesScaled <- scale(na.exclude(series1))
    a_model <- ar(seriesScaled, order.max=1, aic=FALSE, method="yule-walker", demean=TRUE)
    #NAs are returned to series, and the first value of the series is returned (untransformed)

    if (is.null(earlyNAs) && is.null(lateNAs)){

      series2 <- c(seriesScaled[1], na.exclude(a_model$resid))
    } else if ((earlyNAs >= 1) && (lateNAs >= 1)){
      series2 <- c(rep(NA, earlyNAs), seriesScaled[1], na.exclude(a_model$resid), rep(NA, lateNAs))
    } else if ((earlyNAs == 0) && (lateNAs >= 1)){
      series2 <- c(seriesScaled[1], na.exclude(a_model$resid), rep(NA, lateNAs))
    } else if ((earlyNAs >= 1) && (lateNAs == 0)){
      series2 <- c(rep(NA, earlyNAs), seriesScaled[1], na.exclude(a_model$resid))
    }

    return(series2)
  }
}
