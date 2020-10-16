#' lead-lag analysis
#'
#' This function runs lead-lag correlation analysis between multiple timeseries. It outputs a list containing two
#' dataframes that can be used in conjunction with other RingdateR functions to produce summary plots of crossdtes
#' between two samples. It also outputs a data.frame containing the best three matches for each sample.
#' @keywords pairwise_lead_lag
#' @param the_data A dataframe containing the timeseries. First column should contain dates.
#' @param neg_lag An integer to set the lower lag limit.
#' @param pos_lag An integer to set the upper lag limit.
#' @param complete A boolean to classify whther to run the lead-lag over the maximum range of leads and lags
#' @param mode Either 1 or 2. 1 = Pairwise analysis mode, 2 = Chronology analysis mode.
#' @importFrom stats cor.test
#' @export
#' @examples
#' # Load the preloaded example undated data:
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' undated_data <- load_undated(undated_path)\
#' # Detrend the undated data:
#' undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)
#'
#' # Load the preloaded example chronology data:
#' chron_path <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
#' chron_data <- load_chron(chron_path)
#' # Detrend the example chronology data:
#' chrono      <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)
#' # generate an arithemtic mean chronology:
#' chrono      <- data.frame(chrono[,1], rowMeans(chrono[,-1], na.rm = TRUE))
#'
#' # Create a dataframe with the arithemtic mean chronology and detrended undated series
#' chron_n_series <- comb.NA(chrono, undated[,-1], fill = NA)
#'
#'# perform the lead-lag analysis:
#' chron_comp <- lead_lag_analysis(the_data = chron_n_series,
#'                                mode = 2,
#'                                pos_lag= 20,
#'                                neg_lag = -20,
#'                                complete = FALSE)

lead_lag_analysis<-function(the_data, mode = 1, neg_lag = -20, pos_lag = 20, complete = TRUE){
  if (class(neg_lag) != "numeric" || !neg_lag%%1 ==0 || neg_lag > pos_lag){
    stop("Neg_lag needs to be a numeric integer")
  }
  if (class(pos_lag) != "numeric" || !pos_lag%%1 ==0 || pos_lag < neg_lag){
    stop("Pos_lag needs to be a numeric integer")
  }
  if (class(complete) != "logical"){
    stop("complete = missing TRUE/FALSE")
  }
  if (is.null(the_data)){
    stop("Required data.frame is NULL - Enter a data.frame with at least three columns")
  }

  de.tnd<-the_data


  if (mode == 1){multiple <- TRUE
  } else if (mode == 2){multiple <- FALSE}

  calcs<-ncol(de.tnd)
  B<-calcs-1

  repeat{
    calcs<-calcs+B
    B<-B-1
    if(B<1){break}}

#  progress <- Progress$new(session, min=1, max= (calcs * nrow(de.tnd)) * 0.8, style = "notification")
#  on.exit(progress$close())

#  progress$set(message = 'Calculation in progress',
#               detail = 'This may take a while...')

  master_lead_lag <- data.frame(NULL)

  count<- 1

  de.tnd<-the_data
  de.tnd<- subset(de.tnd, !is.na(de.tnd[,1]))

  N_limit<-5
  series_IDs<-colnames(de.tnd)
  no.series<-ncol(de.tnd)

  results_tab<-data.frame()

  a<-2
  b<-a+1

  cross_dat_res<-data.frame()
  repeat{
    repeat{
     # progress$set(value = count)
      #extract the data for the analysis
      years<-de.tnd[,1]
      series_a<-de.tnd[,a]
      series_b<-de.tnd[,b]
      run.cor.dat<-data.frame(years,series_a,series_b)

      series_a_lim<-data.frame(years,de.tnd[,a])
      series_b_lim<-data.frame(years,de.tnd[,b])
      series_a_lim<-subset(series_a_lim, (!is.na(series_a_lim[,1])) & (!is.na(series_a_lim[,2])))
      series_b_lim<-subset(series_b_lim, (!is.na(series_b_lim[,1])) & (!is.na(series_b_lim[,2])))

      series_a_d.of.set<-min(series_a_lim[,1])
      series_a_d.of.d<-max(series_a_lim[,1])

      series_b_d.of.set<-min(series_b_lim[,1])
      series_b_d.of.d<-max(series_b_lim[,1])

      # set up the data for the analysis
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

      correction <- max_pos_lag-max_neg_lag # the correction will be used to adjust the P value to account for the number of correlations calculated

      #neg lag first
      lag<-(max_neg_lag)
      years<-as.numeric(years)
      repeat {
        if(lag<=-1){
          NA_ser<-rep(NA,abs(lag))
          mod_ser_1<-c(NA_ser,series_a)
          mod_ser_2<-c(series_b,NA_ser)
          analysis.data<-comb.NA(mod_ser_1,mod_ser_2, fill = NA)

        } else if (lag==0){
          mod_ser_1<-series_a
          mod_ser_2<-series_b
          analysis.data<-comb.NA(mod_ser_1,mod_ser_2, fill = NA)

        } else if(lag>=1){
          NA_ser<-rep(NA,lag)
          mod_ser_1<-c(series_a,NA_ser)
          mod_ser_2<-c(NA_ser,series_b)
          analysis.data<-comb.NA(mod_ser_1,mod_ser_2, fill = NA)

        }
        ser.2_dates<-data.frame(years,series_b)
        ser.2_dates<-subset(ser.2_dates, complete.cases(ser.2_dates))
        ser.2_dates[,1]<-ser.2_dates[,1]+lag

        analysis.data<-subset(analysis.data, complete.cases(analysis.data))

        if (nrow(analysis.data)<N_limit){
          cor_test<-0
          p_val<-0
          r_val<-0
          t_val<-0
          over<-0
        } else {
          cor_test<-cor.test(analysis.data[,1],analysis.data[,2])
          p_val<-cor_test$p.value * correction
          r_val<-cor_test$estimate
          t_val<-cor_test$statistic
          over<-round(length(analysis.data[,1]),0)
        }
        count <- count + 1
        res_tmp<-data.frame(lag, r_val, p_val, t_val, over, min(ser.2_dates[,1]), max(ser.2_dates[,1]))
        colnames(res_tmp)<-c("lag", "R_Val", "P_Val", "T_val", "Overlap", "First_ring", "Last_ring")
        results_tab<-rbind(results_tab,res_tmp)
        colnames(results_tab)<-c("lag", "R_Val", "P_Val", "T_val", "Overlap", "First_ring", "Last_ring")

        if (lag>=max_pos_lag){break}
        lag<-lag+1
        mod_ser_1<-NULL
        mod_ser_2<-NULL
        ser.2_dates<-NULL

      }

      ######################################################
      ### Produce a results table of the best crossdates ###
      ######################################################

      tab_names<-paste0("ser_1_",series_IDs[a], "_ser_2_",series_IDs[b], "_")
      tmp_res<-results_tab
      res_col_names<-colnames(tmp_res)
      colnames(tmp_res)<-paste0(tab_names,res_col_names)

      # store the full lead lag results in a reactive data frame
      master_lead_lag<-comb.NA(master_lead_lag,tmp_res, fill = NA)

      # store a subet of the results for the large and small results tables
      results_tab<-subset(results_tab, (results_tab[,2]>0))
      ordered<-results_tab[order(results_tab[,3]),]

      chron.strt<-min(de.tnd[,1])
      chron.end<-max(de.tnd[,1])
      chron.rang<-c(chron.strt,chron.end)

      series.a.range<-c(series_a_d.of.set,series_a_d.of.d)

      tab_names<-c("Series_1", "Series_2","First_ring", "Last_ring", "col", "First_lag", "First_R", "First_P", "First_Overlap", "Sec_lag", "Sec_R", "Sec_P", "Sec_Overlap", "Third_lag", "Third_R", "Third_P", "Third_Overlap")

      series_a_date_rang<-data.frame(series_IDs[a],series_IDs[a],series_a_d.of.set,series_a_d.of.d,a,c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA), stringsAsFactors = FALSE)
      colnames(series_a_date_rang)<-tab_names
      if(b==3){cross_dat_res<-rbind(cross_dat_res,series_a_date_rang)
      cross_dat_res_tmp<-data.frame(series_IDs[a], series_IDs[b], ordered[1,6], ordered[1,7],b, ordered[1,1], ordered[1,2], ordered[1,3], ordered[1,5], ordered[2,1], ordered[2,2], ordered[2,3], ordered[2,5], ordered[3,1], ordered[3,2], ordered[3,3], ordered[3,5], stringsAsFactors = FALSE)
      colnames(cross_dat_res_tmp)<-tab_names
      cross_dat_res<-rbind(cross_dat_res,cross_dat_res_tmp)
      } else if (b==a+1){
        cross_dat_res<-rbind(cross_dat_res,series_a_date_rang)
        cross_dat_res_tmp<-data.frame(series_IDs[a], series_IDs[b], ordered[1,6], ordered[1,7], b, ordered[1,1], ordered[1,2], ordered[1,3], ordered[1,5], ordered[2,1], ordered[2,2], ordered[2,3], ordered[2,5], ordered[3,1], ordered[3,2], ordered[3,3], ordered[3,5], stringsAsFactors = FALSE)
        colnames(cross_dat_res_tmp)<-tab_names
        cross_dat_res<-rbind(cross_dat_res,cross_dat_res_tmp)
      } else {
        cross_dat_res_tmp<-data.frame(series_IDs[a], series_IDs[b], ordered[1,6], ordered[1,7], b, ordered[1,1], ordered[1,2], ordered[1,3], ordered[1,5], ordered[2,1], ordered[2,2], ordered[2,3], ordered[2,5], ordered[3,1], ordered[3,2], ordered[3,3], ordered[3,5], stringsAsFactors = FALSE)
        colnames(cross_dat_res_tmp)<-tab_names
        cross_dat_res<-rbind(cross_dat_res,cross_dat_res_tmp)
      }

      results_tab<-data.frame() # clear the results tab
      b<-b+1
      if(b>no.series){break}}
    cross_dat_res_tmp<-data.frame(c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA), c(NA), c(NA), stringsAsFactors = FALSE)
    colnames(cross_dat_res_tmp)<-tab_names

    cross_dat_res<-rbind(cross_dat_res,cross_dat_res_tmp)

    if (multiple){
      a<-a+1
      b<-a+1
      if(a>=no.series){break}
    } else{break}
  }

  cols.num <- seq(3,17, by =1)
  cross_dat_res[cols.num] <- sapply(cross_dat_res[cols.num],as.numeric)

  colnames(cross_dat_res)<-tab_names

  results<-list(cross_dat_res,master_lead_lag)

  return(results)
}
