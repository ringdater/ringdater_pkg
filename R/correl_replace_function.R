#' Evaluate correlations with replacement against a mean chronology
#'
#' Calculate the correlation of each sample, used to construct an arithmetic
#' mean chronology, against the arithmetic mean chronology constructed excluding
#' the selected sample
#'
#' @keywords correlations
#' @param the.data A dataframe containing at least three series and a year column
#' @importFrom stats cor.test
#' @export
#' @examples
#' file_path <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
#' the_data<-load_chron(file_path)
#' correl_replace(the_data)

correl_replace<-function(the.data){
  # check the data are a data.frame
  if (class(the.data) != "data.frame"){
    stop("Error in correl_replace(). Required data are not a data.frame")
  }
  if (ncol(the.data)<=3){
    stop("Error in correl_replace(). Insufficient data to calculate correlations")
  }

  ser<-2
  ser.names<-colnames(the.data)
  colhead<-c("Series ID", "First Ring", "Last ring","R value","P value","Overlap with chronology")
  results<-data.frame()

  for (i in 1:(ncol(the.data)-1)){
    series<-the.data[,ser]
    sub<-the.data[,-ser]

    chron_mean<-rowMeans(sub[,-1], na.rm = TRUE)
    comb<-data.frame(the.data[,1],series,chron_mean)

    just_ser<-data.frame(the.data[,1],series)

    just_ser<-subset(just_ser,complete.cases(just_ser))
    comb<-subset(comb,complete.cases(comb))

    cor_test<-cor.test(series,chron_mean)

    p_val<-cor_test$p.value
    r_val<-cor_test$estimate
    over<-nrow(comb)

    first<-min(just_ser[,1])
    last<-max(just_ser[,1])

    tmp<-data.frame(ser.names[ser],first,last,r_val,p_val,over)
    colnames(tmp)<-colhead
    results<-rbind(results,tmp)
    ser<-ser+1

  }

    colnames(results)<-colhead
    return(results)
}

