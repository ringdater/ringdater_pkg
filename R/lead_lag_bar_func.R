#' Produce a bar chart of leaad-lag correlations and show the best three matches.
#'
#' This function creates are bar chart ofthe lead-lag analyses from RingdateR
#' @keywords lead-lag bar chart
#' @param the_data A dataframe created by lead_lag_analysis() function (lead_lag_analysis()[2]).
#' @param sample_1 A character string containing a sample ID. Must match the original sample IDs perfectly.
#' @param sample_2 A character string containing a sample ID. Must match the original sample IDs perfectly.
#' @export

lead_lag_bar<-function(the_data, sample_1, sample_2){

  run<-TRUE

  # example check for a dataframe with at least 2 columns of data.
  if (class(the_data) != "data.frame"){
    warning("Error in correl_replace(). Required data are not a data.frame")
    run <- FALSE

    # Check thre is enough data in the data.frame
  } else if (ncol(the_data)<=2){
    warning("Error in lead_lag_bar(). Insufficient data to calculate correlations")
    run <- FALSE

  } else if(class(sample_1) != "character"){
    run <- FALSE
    warning("Error in lead_lag_bar(). sample_1 is not a character string.")

  }  else if(class(sample_2) != "character"){
    run <- FALSE
    warning("Error in lead_lag_bar(). sample_2 is not a character string.")

  }

  if (run) {

    samp_names<-paste0("ser_1_",sample_1, "_ser_2_",sample_2, "_")
    master_names<-c("lag", "R_Val", "P_Val", "T_val", "Overlap", "First_ring", "Last_ring")
    master_names<-paste0(samp_names,master_names)

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
}
