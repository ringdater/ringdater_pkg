#' Produce small summary table of a dataframe
#'
#' This function creates the loaded data summary tables.
#' @keywords summary
#' @param the_data A dataframe containing ring width series to be summarised.
#' @importFrom stats sd
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' undated_data <- load_undated(undated_path)
#' load_data_tabs(undated_data)

load_data_tabs <- function(the_data){

  if (is.null(the_data) || class(the_data) != "data.frame"){
    stop("Error in load_data_tabs: the_Data is not a valid data.frame")
  }

  res<-NULL
  ser_name<-c()
  early_year<-c()
  late_year<-c()
  ser_length<-c()
  ser_mean<-c()
  ser_sd<-c()

  for (i in 1: ncol(the_data)){

    tmp_sub<-the_data[,c(1,i)]
    tmp_sub<-subset(tmp_sub, complete.cases(tmp_sub))
    ser_name[i]<-colnames(tmp_sub)[2]
    early_year[i]<-min(tmp_sub[,1])
    late_year[i]<-max(tmp_sub[,1])
    ser_length[i]<-(max(tmp_sub[,1])-min(tmp_sub[,1]))+1
    ser_mean[i]<-mean(tmp_sub[,2])
    ser_sd[i]<-sd(tmp_sub[,2])

  }

  res<-data.frame(ser_name, early_year, late_year, ser_length, ser_mean, ser_sd)[-1,]

  colnames(res)<-c("Series Name", "First ring", "Last ring", "series length", "Series mean", "Series St. dev.")

  return(res)
}
