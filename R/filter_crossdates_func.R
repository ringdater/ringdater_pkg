#' RingdateR shiny server file
#'
#' This function filters the results table created by A dataframe created by lead_lag_analysis() function (lead_lag_analysis()[1])
#'
#' @keywords statistics tests
#' @param the_data A dataframe created by lead_lag_analysis() function (lead_lag_analysis()[1]). Must be a dataframe.
#' @param r_val A numeric value (>=0 and <=1). Correlation coefficient.
#' @param p_val A numeric value (>=0 and <=1). Probablility value.
#' @param overlap An integer >0. This value represents the period of overlap between samples.
#' @param target a column name to be used as the target sample
#' @export

filter_crossdates<-function(the_data, r_val = 0.5, p_val = 0.05, overlap = 50, target = NULL){

  tmp_1<-subset(the_data,(the_data[,1]==target))
  tmp_2<-subset(the_data,(the_data[,2]==target))

  the_data<-rbind(tmp_1,tmp_2)

  the_data<-subset(the_data,(the_data[,7]>=r_val) & (the_data[,8]<=p_val) & (the_data[,9]>=overlap))

  the_data[,7]<-signif(the_data[,7],3)
  the_data[,8]<-signif(the_data[,8],5)
  the_data[,11]<-signif(the_data[,11],3)
  the_data[,12]<-signif(the_data[,12],5)
  the_data[,15]<-signif(the_data[,15],3)
  the_data[,16]<-signif(the_data[,16],5)
  return(the_data)
}

