#' Produces sample distribution plot data
#'
#' Organise data for plotting the distribution plot
#' @keywords data organisation
#' @param the.data a dataframe to be organised so it can be plotted
#' @export
#' @examples
#' chron_path  <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
#' chron_data  <- load_chron(chron_path)
#' plot_data <- dated_line_plot(chron_data)
#' ggplot2::ggplot(data = plot_data, ggplot2::aes(x=dates, y=samp.val, group = name.val))+
#' ggplot2::geom_line() +ggplot2::theme_minimal()

dated_line_plot<-function(the.data){

  if (class(the.data) != "data.frame"){
    stop("Error in dated_line_plot: the_Data is not a valid data.frame")
  }

  ser.names<-colnames(the.data)

  # ORGANISE THE DATA SO THAT THE FIRST COLUMN CONTAINS THE EARLIEST SERIES

  col<-2
  dates<-data.frame()

  for (i in 1:(ncol(the.data)-1)){

    name<-ser.names[col]
    sub<-data.frame(the.data[,1],the.data[,col])
    sub<- subset(sub, complete.cases(sub))
    temp.len<-min(sub[,1])
    temp.res<-data.frame(name,temp.len)
    dates<-rbind(dates,temp.res)
    col<-col+1
  }

  dates<-dates[order(dates[,2]),]
  dates[,1]<-as.character(dates[,1])

  ordered.dat<-data.frame(the.data[,1])
  a<-1

  for (i in 1:nrow(dates)){

    ordered.dat<-cbind(ordered.dat, the.data[[dates[a,1]]])
    a<-a+1
  }

  colnames(ordered.dat)<-c("Year", dates[,1])
  ordered.names<-colnames(ordered.dat)

  res<-data.frame()
  col<-2
  samp<-1

  for (i in 1:(ncol(the.data)-1)){

    name<-ordered.names[col]
    sub<-data.frame(ordered.dat[,1],ordered.dat[,col])

    sub<- subset(sub, complete.cases(sub))

    d.of.s <- min(sub[,1])
    d.of.d <- max(sub[,1])

    dates<-c(d.of.s,d.of.d)
    name.val<-c(name,name)
    samp.val<-c(samp,samp)

    temp<- data.frame(name.val,samp.val,dates)
    res<-rbind(res,temp)

    col<-col+1
    samp<-samp+1
  }

  return(res)
}
