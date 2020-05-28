#' Calculate the Rbar and EPS of a chronology
#'
#' This function uses dplR to calculate the Rbar and EPS of a chronology
#' @keywords Rbar EPS chronology
#' @param the.data A data.frame holding individual series in the aligned chronology
#' @param window A numeric integer used to calculate the running Rbar and EPS values over.
#' @importFrom dplR rwi.stats.running
#' @export

R_bar_EPS<-function(the.data, window = 25){
    if (is.null(the.data)){return(NULL)} else{

    row.names(the.data)<-the.data[,1]

    test<-rwi.stats.running(the.data[,-1], method ="pearson",running.window = TRUE,
                            window.length = window,
                            window.overlap = floor(window / 2),
                            first.start = NULL,
                            round.decimals = 3,
                            zero.is.missing = TRUE)

    res.tab<-data.frame(test$mid.year,test$n.trees,test$n,test$rbar.tot,test$eps)
    return(res.tab)
    }
}
