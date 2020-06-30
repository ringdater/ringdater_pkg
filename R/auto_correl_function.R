#' Calculate the autocorrelation contained in a series
#'
#' Calculate the autocorrelation (1-10th orders) contained in a series.
#' @keywords autocorrelation
#' @param the_data A dataframe containing series to be analysed.
#' @importFrom stats cor
#' @export
#' @examples
#' the_data <-data.frame(x=1:100, y= rnorm(100, 1, 2))
#' auto_correl(the_data)

auto_correl<- function(the_data){

  if (class(the_data) != "data.frame"){
    stop("Error in auto_correl(). Required data are not a data.frame")
  } else if (ncol(the_data)<2){
    stop("Error in auto_correl(). Insufficient data to calculate correlations")
  }

    n.series<-ncol(the_data)
    A<-2
    lag<-c()
    res<-data.frame(c(0:10))


    for (i in 1:(ncol(the_data)-1)){

      dat_1<- the_data[,A]
      dat_2<- the_data[,A]

      comb<-data.frame(dat_1,dat_2)
      comb<-subset(comb, complete.cases(comb))

      r_val<-cor(comb[,1], comb[,2])

      for (k in 1:10){

        NA_ser<-rep(NA,k)

        dat_1<- the_data[A]
        dat_2<- c(NA_ser, the_data[,A])

        comb<-comb.NA(dat_1,dat_2)
        comb<-subset(comb, complete.cases(comb))

        r_val<-c(r_val,cor(comb[,1], comb[,2]))

      }

      res<-cbind(res, r_val)
      r_val<-c()
      A<-A+1

    }
    colnames(res)<-c("lag", colnames(the_data)[-1])

    return(res)
}
