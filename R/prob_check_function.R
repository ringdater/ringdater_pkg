#' Align samples that statistically crossdate
#'
#' Align samples that have been crossdated using a target sample.
#' @keywords problems
#' @param new.chrono A dataframe containing the aligned series to be checked.
#' @param wind An integer to define the window length that the running analyses are calculated over
#' @importFrom dplR corr.rwl.seg


prob_check<-function(new.chrono, wind = 20){

    if(class(new.chrono) != "data.frame"){
      stop("Error in prob_check(): new.chrono should be a data.frame")
    }
    if (class(wind) != "numeric" || wind%%1!=0 || wind < 5){
      stop("Error in prob_check():  The interval is too small, increase the size of the window")
    }

    master <- NULL

    len_test<-new.chrono[!is.na(new.chrono[,1]),]

    even_odd<- wind %% 2 != 0 # check that the correlation window is odd.
    if(even_odd) {bin<-as.numeric(wind)+1 # If it is even add 1 to make it odd.
    } else {bin<-wind}


    if (bin > (0.5*nrow(len_test))) {
      len<-c("Segment length too long")
      res<-data.frame(len)
    } else {

      row.names(new.chrono)<-new.chrono[,1]

      check<-corr.rwl.seg(new.chrono[,-1], seg.length = bin, bin.floor = 10, n = NULL,
                          prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
                          method = c("spearman", "pearson","kendall"),
                          make.plot = FALSE, label.cex = 1, floor.plus1 = FALSE,
                          master = NULL,
                          master.yrs = as.numeric(if (is.null(dim(master))) {
                            names(master)
                          } else {
                            rownames(master)
                          }))

      res<-as.data.frame(check$flags)
      res<-cbind(row.names(res),res)
      colnames(res)<-c("Flagged sample", "Flagged interval")
      res[,2]<-as.character(gsub("[.]", " to ",  res[,2]))

      if(nrow(res)<1){
        Flagged_samples<-c("No problems detected")
        res<-data.frame(Flagged_samples)
      }
    }

    return(res)

}




