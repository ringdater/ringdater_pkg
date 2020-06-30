#' Check for problems in the loaded data.
#'
#' This function checks if there are any problems with data that have been loaded into RingdteR
#' before running the analysis. It returns an code (0-2) denoting if there are detected problems
#' with the loaded data.
#'
#' Output codes:
#' 0 = No problems with the data.
#' 1 = The first column in the dataframe conatins NA values.
#' 2 = The loaded data are discontinuous/contain missing values.
#'
#' @keywords data quality
#' @param the_data A dataframe containing the series to be used in RingdateR
#' @export
#' @examples
#' the_data <- data.frame(x = 1:10, y = 1:10)
#' loaded_data_check(the_data)
#' # Return of 0 means no errors

loaded_data_check<-function(the_data){

  # example check for a dataframe with at least 2 columns of data.
  if (class(the_data) != "data.frame"){
    stop(" Required data are not a data.frame")
  }
  if (ncol(the_data)<2){
    stop("Insufficient data")
  }

    prob_cd <- 0 # return zero if there are no problems

    max_date<-c()
    min_date<-c()
    a<-2
    rep_lim<-ncol(the_data)-1

    for (i in 1:rep_lim){
      sub<-the_data[,c(1,a)]
      sub<-subset(sub, !is.na(sub[,2]))

      if (any(is.na(sub[,1]))==TRUE){prob_cd<-1
      break()}

      max_tmp<-max(sub[,1], na.rm=TRUE)
      min_tmp<-min(sub[,1], na.rm=TRUE)

      sub<-the_data[,c(1,a)]
      sub<-subset(sub, sub[,1]>=min(min_tmp) & sub[,1]<=max(max_tmp))

      if (any(is.na(sub[,2]))==TRUE){prob_cd<-2
        break()
      } else {
      a<-a+1}
    }

      if (prob_cd == 0){
        return(prob_cd)
      } else if (prob_cd == 1) {
       return(prob_cd)
      } else if (prob_cd == 2) {
       return(prob_cd)}

}

