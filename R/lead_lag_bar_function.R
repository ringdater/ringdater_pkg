#' Produce a bar chart of leaad-lag correlations and show the best three matches.
#'
#' This function creates are bar chart ofthe lead-lag analyses from RingdateR
#' @keywords lead-lag bar chart
#' @param the_data A dataframe created by lead_lag_analysis() function (lead_lag_analysis()[2]).
#' @param sample_1 A character string containing a sample ID. Must match the original sample IDs perfectly.
#' @param sample_2 A character string containing a sample ID. Must match the original sample IDs perfectly.
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
#' # chron_n_series <- comb.NA(chrono, undated[,-1], fill = NA)
#' # chron_n_series[1:6,1:6]
#' #
#' # chron_comp <- lead_lag_analysis(the_data = chron_n_series,
#' #                                 mode = 2,
#' #                                 complete = TRUE,
#' #                                 shiny = FALSE)
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
#' lead_lag_bar(the_data = as.data.frame(chron_comp[2]),
#'              sample_1 = "mean_chronology",
#'              sample_2 = "sample_a")

lead_lag_bar<-function(the_data, sample_1, sample_2){

  if (class(the_data) != "data.frame"){
    stop("Error in correl_replace(). Required data are not a data.frame")
  }
  if (ncol(the_data)<=2){
    stop("Error in lead_lag_bar(). Insufficient data to calculate correlations")
  }
  if(class(sample_1) != "character"){
    stop("Error in lead_lag_bar(). sample_1 is not a character string.")
  }
  if(class(sample_2) != "character"){
    stop("Error in lead_lag_bar(). sample_2 is not a character string.")
  }

    samp_names<-paste0("ser_1_",sample_1, "_ser_2_",sample_2, "_")
    master_names<-c("lag", "R_Val", "P_Val", "T_val", "Overlap", "First_ring", "Last_ring")
    master_names<-paste0(samp_names,master_names)

  if(!master_names[1] %in% colnames(the_data)){
    stop("Error in lead_lag_bar(): Resilts not found for selected samples")
  }

    lag <-the_data[[master_names[1]]]
    R_Val<-the_data[[master_names[2]]]
    P_Val <-the_data[[master_names[3]]]
    T_val <-the_data[[master_names[4]]]
    Overlap <-the_data[[master_names[5]]]
    First_ring	<-the_data[[master_names[6]]]
    Last_ring <-the_data[[master_names[7]]]
    selected<-data.frame(lag, R_Val, P_Val, T_val, Overlap, First_ring, Last_ring)

    # remove neg values
    selected<-subset(selected,(selected[,2]>0))

    ordered<-selected[order(selected[,3]),]

    best<-ordered[1,]
    second<-ordered[2,]
    third<-ordered[3,]

    plot1<-ggplot()+
      geom_bar(data=selected, aes(x=selected[,1], weight= (selected[,4])), fill= "black", na.rm=T) +
      R_dateR_theme(text.size = 12, line.width = 1) +ylab("T_val") + xlab("Lag (Year)") +
      geom_bar(data=best, aes(x=best[,1], weight= (best[,4])), fill= "red", na.rm=T) +
      geom_bar(data=second, aes(x=second[,1], weight= (second[,4])), fill= "blue", na.rm=T) +
      geom_bar(data=third, aes(x=third[,1], weight= (third[,4])), fill= "green", na.rm=T) +
      scale_x_continuous(breaks = x.scale.bar(round(min(as.numeric(selected[,1]), na.rm = TRUE), -1), round(max(as.numeric(selected[,1]), na.rm = TRUE), -1)))

    return(plot1)

}
