#' running lead_lag analysis
#'
#' This function performs running lead-lag correlation analysis between two time series.
#' The output dataframe is used to produce the heatmaps
#' @keywords pairwise_lead_lag
#' @param the_data A dataframe containing the timeseries. First column should contain dates.
#' @param s1 the name of a sample - must match a column name exactly
#' @param s2 the name of a sample - must match a column name exactly
#' @param neg_lag An integer
#' @param pos_lag An integer
#' @param complete A boolean to classify whether to run the lead-lag over the maximum range of
#' leads and lags between the two selected samples
#' @param win The window with which the running correlations are calculated over
#' @importFrom zoocat rollcor
#' @importFrom zoo rollmean
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' undated_data <- load_undated(undated_path)
#' undated_data <- name_check(undated_data)
#' undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)
#' running_lead_lag(the_data = undated, s1 = colnames(undated)[2], s2 = colnames(undated)[3],
#'                  neg_lag = -20, pos_lag = 20, win = 21, complete = FALSE)

running_lead_lag<-function(the_data, s1, s2, neg_lag = -20, pos_lag = 20, win = 21, complete = TRUE){
  if(is.null(the_data) || class(the_data) != "data.frame"){
    stop("Warning. Error in running_lead_lag. the_data is not a valid data.frame")
  }
  if(!s1 %in% colnames(the_data)){
    stop("Error in running_lead_lag: s1 is not a valid sample ID")
  }
  if(!s2 %in% colnames(the_data)){
    stop("Error in running_lead_lag: s2 is not a valid sample ID")
  }
  if(class(pos_lag) != "numeric" || pos_lag < neg_lag || pos_lag%%1 !=0){
    stop("Error in running_lead_lag: pos_lag should be an numeric integer with a value greater than leg_lag")
  }
  if(class(neg_lag) != "numeric" || neg_lag > pos_lag || neg_lag%%1 !=0){
    stop("Error in running_lead_lag: neg_lag should be an numeric integer with a value less than pos_lag")
  }
  if(class(win) != "numeric" || win%%1 !=0 || win < 5){
    stop("Error in running_lead_lag: win should be a numeric integer")
  }
  if (class(complete) !="logical"){
    stop("Error in running_lead_lag: complete should be TRUE or FALSE")
  }

    even_odd<- win %% 2 == 0 # check that the correlation window is odd.
    if(even_odd) {win<-as.numeric(win)+1 # If it is even add 1 to make it odd.
    } else {win<-win}

    a<-the_data[s1]
    b<-the_data[s2]
    the_data<-data.frame(the_data[,1],a,b, stringsAsFactors = TRUE)
    colnames(the_data)<-c("years","A","B")
    series_ID<-colnames(the_data)

    N_limit<-win

    ###########################################################

    run_cor_res<-data.frame()

    #extract the data for the analysis
    years<-the_data[,1]
    series_a<-the_data[,2]
    series_b<-the_data[,3]
    run.cor.dat<-data.frame(years,series_a,series_b)

    ser_a_len<-length(subset(series_a, (!is.na(series_a))))
    ser_b_len<-length(subset(series_b, (!is.na(series_b))))

    pos_lag_lim<- max(c(ser_a_len,ser_b_len))
    neg_lag_lim<- -max(c(ser_a_len,ser_b_len))

    if(complete){pos_lag<-pos_lag_lim   # tells the analysis to run over the maximum allowable lead-lag
    neg_lag<-neg_lag_lim}

    if (pos_lag>pos_lag_lim){max_pos_lag<-pos_lag_lim
    } else {max_pos_lag<-pos_lag}

    if (neg_lag<neg_lag_lim){max_neg_lag<-neg_lag_lim
    } else {max_neg_lag<-neg_lag}

    # set up the data for the analysis

    lag<-max_neg_lag

    repeat {
 #     progress$set(value = prog_count)
      if(lag<=-1){
        NA_ser<-rep(NA,abs(lag))
        yr_mod<-c(NA_ser,years)
        mod_ser_1<-c(NA_ser,series_a)
        mod_ser_2<-c(series_b,NA_ser)
        analysis.data<-comb.NA(mod_ser_1,mod_ser_2, fill = NA)
      } else if (lag==0){
        yr_mod<-years
        mod_ser_1<-series_a
        mod_ser_2<-series_b
        analysis.data<-comb.NA(mod_ser_1,mod_ser_2, fill = NA)
      } else if(lag>=1){
        NA_ser<-rep(NA,lag)
        yr_mod<-c(years,NA_ser)
        mod_ser_1<-c(series_a,NA_ser)
        mod_ser_2<-c(NA_ser,series_b)
        analysis.data<-comb.NA(mod_ser_1,mod_ser_2, fill = NA)
      }

      len_test<-subset(analysis.data, complete.cases(analysis.data[,1:2]))

      if(nrow(len_test)<=win){NULL

      } else {
        cor_test<-rollcor(analysis.data[,1],analysis.data[,2], width = win, show =F)
        cor_year<-rollmean(yr_mod, k = win)
        lag_ser<- rep(lag,length(cor_year))

        run_cor_tmp<-comb.NA(cor_year, lag_ser, cor_test, fill = NA)
        run_cor_tmp<-subset(run_cor_tmp, complete.cases(run_cor_tmp))
        colnames(run_cor_tmp)<-c("year", "lag", "R val")

        run_cor_res<-rbind(run_cor_res,run_cor_tmp)
        colnames(run_cor_res)<-c("year", "lag", "R val")

        mod_ser_1<-NULL
        mod_ser_2<-NULL
      }
      if (lag>=max_pos_lag){break}
      lag<-lag+1
 #     prog_count<-prog_count+1

    }
    if(nrow(run_cor_res)<15){run_cor_res<-NULL}

    return(run_cor_res)
    run_cor_res<-data.frame(NULL)
}







