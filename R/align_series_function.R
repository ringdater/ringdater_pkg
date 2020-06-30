#' Align samples that statistically crossdate
#'
#' Align samples that have been crossdated using a target sample.
#' @keywords Crossdate
#' @param the_data A dataframe containing the individual seeries to be aligned.
#' @param cross_dates A dataframe containing the results of the crossdating (generated using lead_lag_analysis()[1]).
#' These data should be filtered, using filter_crossdates(), so this dataframe only contains significant ccrossdates
#' to the selected target sample.
#' @param sel_target A sample ID (column name) entered as a character string. This must be the same as the target sample used in the filter_crossdates() function.
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' undated_data <- load_undated(undated_path)
#' undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)
#'
#' chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
#' chron_data  <- load_chron(chron_path)
#' chrono      <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)
#' chrono      <- data.frame(chrono[,1], rowMeans(chrono[,-1], na.rm = TRUE))
#' colnames(chrono)<-c("year", "mean_chronology")
#'
#' chron_n_series <- comb.NA(chrono, undated[,-1], fill = NA)
#'
#' exmp_analysis_1 <- system.file("extdata", "chron_comp_1.csv", package="ringdater")
#' chron_comp_1 <- read.csv(exmp_analysis_1, stringsAsFactors = FALSE, header = TRUE)
#'
#' exmp_analysis_2 <- system.file("extdata", "chron_comp_2.csv", package="ringdater")
#' chron_comp_2 <- read.csv(exmp_analysis_2, stringsAsFactors = FALSE, header = TRUE)
#'
#' chron_comp <- list(chron_comp_1,chron_comp_2)
#'
#' filtered_data <- filter_crossdates(the_data = as.data.frame(chron_comp[1]),
#'                                    r_val = 0.4,
#'                                    p_val = 0.05,
#'                                    overlap = 30,
#'                                    target = "mean_chronology")
#'
#' aligned_data <- align_series(the_data = chron_n_series,
#'                              cross_dates = filtered_data,
#'                              sel_target = "mean_chronology")

align_series<-function(the_data, cross_dates, sel_target){
  if (class(sel_target) != "character"){
    stop("Error in align_series(). sel_target is not a character string.")
  }
  if (class(the_data) != "data.frame"){
    stop("Error in align_series(). Required data are not a data.frame")
    }
  if (ncol(the_data)<=2){
    stop("Error in align_series(). Insufficient data in the_data")
  }
  if (nrow(the_data)<=1){
    stop("Error in align_series(). Insufficient data in the_data")
  }
  if (class(cross_dates) != "data.frame"){
    stop("Error in align_series(). Required cross_dates are not a data.frame")
  }
  if (ncol(cross_dates)<=2){
    stop("Error in align_series(). Insufficient data in cross_dates")
  }

  target<-data.frame(the_data[,1],the_data[[sel_target]])
  target<-subset(target, complete.cases(target))
  colnames(target)<-c("years",sel_target)

  targ_min_yr<-min(target[,1])
  targ_max_yr<-max(target[,1])

  n.series<-nrow(cross_dates)

  ser_dates<-data.frame(sel_target, targ_min_yr, targ_max_yr)
  colnames(ser_dates)<-c("series_ID","Min_year", "max_year")

  for (i in 1:n.series){ # i is going to relate to the row number in crossdates table

    #get the data for the sample to align
    if (cross_dates[i,1]==sel_target){
      new_sample<-as.character(cross_dates[i,2])
      lag<-cross_dates[i,6]
    } else {
      new_sample<-as.character(cross_dates[i,1])
      lag<--cross_dates[i,6]
    }

    new_samp_dat<-data.frame(the_data[,1],the_data[[new_sample]])
    new_samp_dat<-subset(new_samp_dat, complete.cases(new_samp_dat))
    new_samp_dat[,1]<-new_samp_dat[,1]+lag

    min_new_samp<-min(new_samp_dat[,1])
    max_new_samp<-max(new_samp_dat[,1])

    tmp<-data.frame(new_sample,min_new_samp,max_new_samp)
    colnames(tmp)<-c("series_ID","Min_year", "max_year")

    ser_dates<-rbind(ser_dates,tmp)
    tmp<-NULL
  }

  min_new_yr<-min(ser_dates[,2])
  max_new_yr<-max(ser_dates[,3])

  new_years<- c(min_new_yr:max_new_yr)

  aligned_data<- data.frame(new_years)
  sample_ID<-as.character(ser_dates[,1])

  for (i in 1:length(sample_ID)){

    tmp<-the_data[[sample_ID[i]]]
    tmp<-subset(tmp,(!is.na(tmp)))

    min_date<-ser_dates[i,2]
    dif<- abs(min_date - min_new_yr)

    if (dif>=1){
      NA_ser<-rep(NA,dif)
      tmp<-c(NA_ser,tmp)
    }
    aligned_data<-comb.NA(aligned_data,tmp, fill = NA)
    tmmp<-NULL
  }

  colnames(aligned_data)<-c("Year", sample_ID)

  return(aligned_data)
}
