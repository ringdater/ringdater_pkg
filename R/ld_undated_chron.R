#' Load multiple undated chronologies into a single dataframe
#'
#' Create a dataframe containing the arithmetic mean chronologies from multiple files
#' containing either two column chronologies or files containing multiple series to be
#' compiled as a chronology.
#'
#' @keywords RingdateR load
#' @param files A list of file paths
#' @param series_names A list of file names. If not using the shiny app,  series names are automatically generated in the function so do not need to be parsed.
#' @param pair_detrend A boolean to dictate whether detrending should be applied or not. Set to false by default.
#' @param detrending_select Select the method for detrending. 1 = Do nothing, 2 = Z-scores, 3 = spline detrending, 4 = Mod. negative exponentia, 5 = Friedman, 6 = ModHugershoff, 7 = First difference. Spline is slected by default.
#' @param splinewindow An integer to define the window to be used if spline detrending is selected. Set to 21 years by default.
#' @param powerT A boolean to dictate whether or not to power transform the data.
#' @param shiny A boolean to let the function know if it is being run in the shiny app or in the console.
#' @importFrom readxl read_excel
#' @importFrom dplR read.rwl
#' @importFrom dplR chron
#' @export
#' @examples
#' chron_path  <- system.file("extdata", "undated_chron.xlsx", package="ringdater")
#' chron_data  <- ld_undated_chron(chron_path)

ld_undated_chron<-function(files, series_names = NULL, pair_detrend = FALSE, detrending_select = 3, splinewindow = 21, powerT = FALSE, shiny = FALSE){
  run<-TRUE

  if (class(pair_detrend) != "logical"){
    run <- FALSE
    warning("Warning in ld_undated_chron(): pair_detrend should be a TRUE/FALSE.")
    }
  if (detrending_select %in% c(1:7) == FALSE){
    warning("Error in ld_undated_chron(). detrending_select must be a numeric integer from 1 to 7. detrending_select set to 3 (spline detrending).")
    detrending_select <- 3
  }
  if (splinewindow %in% c(5:500) == FALSE){
    warning("Error in ld_undated_chron(). splinewindow must be a numeric integer from 5 to 500. splinewindow set to 21.")
    splinewindow <- 21
  }
  if (class(pair_detrend) != "logical"){
    run <- FALSE
    warning("Warning in ld_undated_chron(): powerT should be a TRUE/FALSE.")
  }

  if (run){

    undated_df_data <- data.frame(NULL)

    file_detect<-function(x,n){
      substr(x, nchar(x)-n+1, nchar(x))
    }

    for (k in 1:length(files)){
      ftype<-file_detect(files[k],3)

      # Generate the file name, if in shiny bring in from the file.load, otherwise use base R to generate it.
      if (shiny){
        series<-series_names[k]
      } else {
        series<-basename(files[k])
      }

      # load the data based on the file format
      if (ftype =="rwl"){
        the_data<-read.rwl(files[k])
        the_data<-data.frame(row.names(the_data), the_data)
        series<-as.character(gsub(".rwl", "", series))
      } else if (ftype == "csv"){
        the_data<-read.csv(files[k], header = TRUE, stringsAsFactors = FALSE)
        series<-as.character(gsub(".csv", "", series))
      } else if (ftype == "txt"){
        the_data<-read.csv(files[k], header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        series<-as.character(gsub(".txt", "", series))
      }  else if (ftype == "lsx"){
        the_data<-as.data.frame(read_excel(files[k], sheet = 1, na ="NA" ))
        series<-as.character(gsub(".xlsx", "", series))
      }

      # remove characters that can not be used in column IDs
      series<-as.character(gsub("[-.]", "_", series))

      # extract the year column
      years<-the_data[,1]

      # if detrending is not being applied set detrending method to do not apply
      if (pair_detrend){detrend_opt<-detrending_select
      } else if (pair_detrend == FALSE){detrend_opt <- 1}

      # Detrend the data
      detrended.data <- normalise(the.data = the_data, detrend_opt, splinewindow)

      row.names(detrended.data)<-detrended.data[,1]
      detrended.data<-as.data.frame(detrended.data[,-1])

      # detect if the chronology contains multiple samples or is already a mean chronology
      # Ifthe chronology is multiple individual samples, calculate the arithmetic meanchronology
      if (ncol((detrended.data))>1){
        tmp <- chron(detrended.data)
        tmp <- data.frame(as.numeric(row.names(tmp)),tmp[,-2])
      } else {
        # if it is a mean chronology already, then extract the mean chronology
        tmp <- detrended.data[,1]
        tmp <- data.frame(c(1:length(tmp)-1),tmp)
      }

      # Determine if this is the first undated chronology to be loaded
      if (ncol(undated_df_data) < 2) {
        # IF it is the first, then put it in the undated_df_data frame and then remove the NA column
        undated_df_data <- comb.NA(undated_df_data, tmp, fill = NA)
        undated_df_data<-undated_df_data[,-1] # get rid of the NA value column
        colnames(undated_df_data)<-c("ring", series)
        # as these data are detrended when loaded, stick the data straight into the detrended_undated_df_data data.frame
        detrended_undated_df_data<-undated_df_data

      } else {
        # If it isn't the first undated dated chronology loaded, get the names for the series that have already been loaded
        old_names<-colnames(undated_df_data)
        # Add the new undated chronology to the data.frame and then recombine the column names
        undated_df_data <- comb.NA(undated_df_data, tmp[,2], fill = NA)
        colnames(undated_df_data)<-c(old_names,series)
        # as these data are detrended when loaded, stick the data straight into the detrended_undated_df_data data.frame
        detrended_undated_df_data<-undated_df_data
      }

      # make sure that the first column contains a continuous ring count for the full length of all the loaded undated chronologies.
      undated_df_data[,1]<-c(1:nrow(undated_df_data))

    }

    undated_df_data[,1]<-c(undated_df_data[1,1]:(undated_df_data[1,1] + nrow(undated_df_data) - 1))

    return(undated_df_data)
  }
}

# once run set the undated_df_data and detrended_undated_df_datato = the outpoutted dataframe



