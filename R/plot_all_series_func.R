#' Plot all data in dataframe along with the arithmetic mean chronology
#'
#' Generate a plot of all the aligned data along with the arithmetic mean chronology
#' @keywords plot
#' @param aligned_data A data frame containing multiple ring width series aligned in time. First column should be years/ring count
#' @export
#' @examples
#' chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
#' chron_data  <- load_chron(chron_path)
#' chron_data  <- name_check(chron_data)
#' chrono      <- normalise(the.data = chron_data, detrending_select = 3, splinewindow = 21)
#'
#' plot_all_series(chrono)

plot_all_series<-function(aligned_data){

  # example check for a dataframe with at least 2 columns of data.
  if (class(aligned_data) != "data.frame"){
    warning("Error in plot_all_series(). Required data are not a data.frame")
    return(NULL)
    # Check thre is enough data in the data.frame
  } else if (ncol(aligned_data)<=2){
    warning("Error in plot_all_series(). Insufficient data.")
    return(NULL)
  } else {

    new_chron_mean<-rowMeans(aligned_data[,-1], na.rm = TRUE)
    chron_dat<-data.frame(aligned_data[,1],new_chron_mean)

    a<-1
    b<-2
    count<-1
    new<-data.frame()

    while(b<=ncol(aligned_data)){
      tmp<-data.frame(aligned_data[,a],aligned_data[,b])
      tmp<-subset(tmp, (!is.na(tmp[,1]) & !is.na(tmp[,2])))
      group<-as.character(rep(count,length(tmp[,1])))
      tmp<-cbind(tmp,group)
      colnames(tmp)<-c("col_1","col_2","group")
      new<-rbind(new,tmp)
      b<-b+1
      count<-count+1
    }
    x.lab<-"Years"

    plot1<-  ggplot()+
      geom_line(data = new, aes(x = new[,1], y = new[,2], group = new[,3]), size = 0.5, alpha=0.5, na.rm=TRUE) +
      R_dateR_theme(text.size = 12, line.width = 1,l=20) +
      geom_line(data=chron_dat, aes(x=chron_dat[,1], y=chron_dat[,2]), colour = "red", size = 0.5, na.rm=TRUE) +
      ylab("Standardised increment width") + xlab(x.lab) +
      scale_x_continuous(breaks = x.scale.bar(round(min(new[,1]),-1),round(max(new[,1]),-1)))

    return(plot1)
  }
}
