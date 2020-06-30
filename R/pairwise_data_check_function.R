#' pairwise data check
#'
#' Check if the are any problems with the loaded undated series
#'
#' @keywords RingdateR load
#' @param the_data A data frame containing the undated series


pairwise_data_check<-function(the_data){
  if (class(the_data) != "data.frame") {
    stop("Error in pairwise_data_check: the_data must be a data.frame.")
  }
  if (ncol(the_data) < 2) {
    stop("Error in pairwise_data_check: insufficient data in the_data.")
  }

    years<-the_data[,1]

    if(years[1]>years[2]){
      shinyalert("Warning!", "Year Values in your data go in the wrong direction. Please fix this and reload the data", type = "error")

      return(NULL)
    }

    test<-loaded_data_check(the_data)

    if (test == 0){
      max_date<-c()
      min_date<-c()
      a<-2
      rep_lim<-ncol(the_data)-1

      for (i in 1:rep_lim){
        sub<-the_data[,c(1,a)]
        sub<-subset(sub, complete.cases(sub))
        max_tmp<-max(sub[,1])
        min_tmp<-min(sub[,1])
        max_date<-c(max_date,max_tmp)
        min_date<-c(min_date,min_tmp)
        a<-a+1
      }

      the_data<-subset(the_data, the_data[,1]>=min(min_date) & the_data[,1]<=max(max_date))

      return(the_data)
    } else if (test == 1) {

      shinyalert("Warning!", "Check the data that was loaded, possible issue with your year column", type = "error")

      return(NULL)

    } else if (test==2) {

      shinyalert("Warning!", "Some of the data contain missing values", type = "error")

      return(NULL)

    }

}
