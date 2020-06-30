#' RingdateR shiny server file
#'
#' This function filters the results table created by A dataframe created by lead_lag_analysis() function (lead_lag_analysis()[1])
#'
#' @keywords statistics tests
#' @param the_data A dataframe created by lead_lag_analysis() function (lead_lag_analysis()[1]). Must be a dataframe.
#' @param r_val A numeric value (>=0 and <=1). Correlation coefficient.
#' @param p_val A numeric value (>=0 and <=1). Probability value.
#' @param overlap An integer >0. This value represents the period of overlap between samples.
#' @param target a column name to be used as the target sample
#' @export
#' @examples
#' # undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' # undated_data <- load_undated(undated_path)
#' # undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)
#' #
#' # chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
#' # chron_data  <- load_chron(chron_path)
#' # chrono      <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)
#' # chrono      <- data.frame(chrono[,1], rowMeans(chrono[,-1], na.rm = TRUE))
#' # colnames(chrono)<-c("year", "mean_chronology")
#' #
#' # chron_n_series <- comb.NA(chrono, undated[,-1], fill = NA)
#' # chron_n_series[1:6,1:6]
#' #
#' # chron_comp <- lead_lag_analysis(the_data = chron_n_series,
#' #                                 mode = 2,
#' #                                 complete = TRUE,
#' #                                 shiny = FALSE)
#'
#' # For speed, the results of the analyses above are imported as two CSV files and put
#' # together into a list (chron_comp).
#' exmp_analysis_1 <- system.file("extdata", "chron_comp_1.csv", package="ringdater")
#' chron_comp_1 <- read.csv(exmp_analysis_1, stringsAsFactors = FALSE, header = TRUE)
#'
#' exmp_analysis_2 <- system.file("extdata", "chron_comp_2.csv", package="ringdater")
#' chron_comp_2 <- read.csv(exmp_analysis_2, stringsAsFactors = FALSE, header = TRUE)
#'
#' chron_comp<-list(chron_comp_1,chron_comp_2)
#'
#' filtered_data <- filter_crossdates(the_data = as.data.frame(chron_comp[1]),
#'                                   r_val = 0.4,
#'                                   p_val = 0.05,
#'                                   overlap = 30,
#'                                   target = "mean_chronology")

filter_crossdates<-function(the_data, r_val = 0.5, p_val = 0.05, overlap = 50, target = NULL){

  if (class(the_data) !=  "data.frame"){
    stop("Error in filter_crossdates: the_data is not of class data.frame")
  }
  if (class (r_val) != "numeric" || r_val < 0 || r_val > 1){
    stop("Error in filter_crossdates: r_val should be numeric value > 0 and < 1")
  }
  if (class (p_val) != "numeric" || p_val < 0 || p_val > 1){
    stop("Error in filter_crossdates: p_val should be numeric value > 0 and < 1")
  }
  if (class (overlap) != "numeric" || overlap %%1!=0 || overlap < 1){
    stop("Error in filter_crossdates: overlap should be numeric integer")
  }
  if (!target %in% the_data[,1] || !target %in% the_data[,2]){
    stop("Error in filter_crossdates: target must be a valid sample ID")
  }

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

