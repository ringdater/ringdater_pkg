#' Align crossdated series to series contained in a chronology
#'
#' This function replaces the arithmetic mean chronology, which has been aligned with undated series,
#' with the individual series used to generate the arithmetic mean chronology.
#'
#' @keywords crossdating
#' @param the.data The dataframe containing the aligned data with the arithemtic mean chronology (in the second column)
#' and the undated series that hve been aligned in time with the mean chronology.
#' @param chrono A dataframe containing the individual series used to construct the arithmetic mean chronology
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' undated_data <- load_undated(undated_path)
#' undated      <- normalise(the.data = undated_data, detrending_select = 3, splinewindow = 21)
#'
#' chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
#' chron_data  <- load_chron(chron_path)
#' chrono_det  <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)
#' chrono      <- data.frame(chrono_det[,1], rowMeans(chrono_det[,-1], na.rm = TRUE))
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
#'                                   r_val = 0.4,
#'                                   p_val = 0.05,
#'                                   overlap = 30,
#'                                   target = "mean_chronology")
#'
#' # Align the crossdated series to the mean chronology
#' aligned_data <- align_series(the_data = chron_n_series,
#'                              cross_dates = filtered_data,
#'                              sel_target = "mean_chronology")
#' # REplace the arithmetic mean chronology with the individual timeseries
#' # used to generate the chronology
#' fully_aligned <- align_to_chron(the.data = aligned_data,
#'                                chrono = chrono_det)


align_to_chron<-function(the.data, chrono){

  if (class(the.data) != "data.frame"){
    warning("Error in align_to_chron(). Required data (the.data) are not a data.frame")
  }
  if (ncol(the.data)<=2){
    warning("Error in align_to_chron(). Insufficient data (the.data) to align series")
  }
    # example check for a dataframe with at least 2 columns of data.
  if (class(chrono) != "data.frame"){
      warning("Error in align_to_chron(). Required chrono data are not a data.frame")
  }
  if (ncol(chrono)<2){
      warning("Error in align_to_chron(). Insufficient data in chrono to align series")
  }

    the.data<-the.data[,-2] # the second column would contain the arithemtic mean chronology so remove it

     # set the date range for the new aligned data.frame
    min.the.data<- min(as.numeric(the.data[,1]))
    max.the.data<- max(as.numeric(the.data[,1]))
    chron.min<- min(chrono[,1])
    chron.max<- max(chrono[,1])

    # generate a new year colum that spans the full range of dates of the new chronology data
    new.years<-c(min(c(min.the.data,chron.min)):max(c(max.the.data,chron.max)))
    new.chrono<-data.frame(new.years)

    # add thechronology dsata to the data frame
    if(chron.min>min.the.data){
      NA.frame<-data.frame(matrix(NA, nrow = abs(chron.min-min.the.data), ncol = (ncol(chrono)-1)))
      colnames(NA.frame)<-colnames(chrono[,-1])
      chrono.tmp<-rbind(NA.frame,chrono[,-1])
      new.chrono<-comb.NA(new.chrono,chrono.tmp, fill=NA)
    } else {new.chrono<-comb.NA(new.chrono,chrono[,-1], fill=NA)}

    # add the new series to the data frame
    if(chron.min<min.the.data){
      NA.frame<-data.frame(matrix(NA, nrow = abs(min.the.data-chron.min), ncol = (ncol(the.data)-1)))
      colnames(NA.frame)<-colnames(the.data[,-1])
      the.data.tmp<-rbind(NA.frame,the.data[,-1])
      new.chrono<-comb.NA(new.chrono,the.data.tmp, fill=NA)
    } else {new.chrono<-comb.NA(new.chrono,the.data[,-1], fill=NA)}

    chron.names<-colnames(chrono)
    ser.nam<-colnames(the.data)[-1]
    colnames(new.chrono)<-c(chron.names,ser.nam)

    return(new.chrono)
}
