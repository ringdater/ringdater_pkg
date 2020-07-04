#' Load multiple undated series into a single data frame
#'
#' This function creates a dataframe containing the data from multiple files loaded at one.
#' @keywords RingdateR
#' @param files A list of set of files pathsas a character strings
#' @param series_names The file names, only used in the shiny server function.
#' @param shiny a logical to set series names. If running in cosole can be ignored.
#' @param col1 A character string to label the first column (e.g. Year or Ring)
#' @importFrom magrittr %<>%
#' @importFrom dplR read.rwl
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' load_undated(undated_path)

load_undated<-function(files, col1 = "ring", series_names = NULL, shiny = FALSE){

    loading_df_data <- data.frame(NULL)
    undated_df_data <- data.frame(NULL)

    file_detect<-function(x,n){
      substr(x, nchar(x)-n+1, nchar(x))
    }

    for (k in 1:length(files)){
      # get the file name ready to be used as sample ID
      ftype<-file_detect(files[k],3)

      acc_ftypes <- c("txt", "pos", "csv", "lsx", "rwl")

      if (!ftype %in% acc_ftypes){
        stop(paste0("Error in load_undated. File was unsupported file type. \n
                    Problem file: ", files[k]))
      }

      # Generate the file name, if in shiny bring in from the file.load, otherwise use base R to generate it.
      if (shiny){
        series<-series_names[k]
      } else {
        series<-basename(files[k])
      }

      # loads a two column txt without headers
      if (ftype =="txt"){
        # load the two column txt file without headers
          loading_df_data<-read.csv(files[k], header = FALSE, sep = "\t")
        # if the file contains more than two series with headers, the data will be factors, therefor reload it with headers
        if (class(loading_df_data[,1]) == "factor"){
          loading_df_data<-read.csv(files[k], header = TRUE, sep = "\t")
        }
        if (ncol(loading_df_data) == 2){
          loading_df_data<-subset(loading_df_data, complete.cases(loading_df_data))
          series<-as.character(gsub(".txt", "", series))
          colnames(loading_df_data)<-c(col1,series)
        }
      } else if (ftype =="pos"){
        loading_df_data<-load_pos(file_path = files[k])
        series<-as.character(gsub(".pos", "", series))
        colnames(loading_df_data)<-c(col1,series)

      } else if (ftype=="lps"){
        series<-as.character(gsub(".lps", "", series))
        loading_df_data <- load_lps(series = series, path = files[k])


      } else if (ftype=="pos"){
        loading_df_data<-as.data.frame(read_excel(files[k], sheet = 1, na ="NA" ))

      } else  if (ftype=="csv"){
        loading_df_data<-read.csv(files[k], header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

      } else if (ftype == "lsx"){
        loading_df_data<-as.data.frame(read_excel(files[k], sheet = 1, na ="NA" ))

      } else  if(ftype=="rwl"){
        loading_df_data<-read.rwl(files[k])
        loading_df_data <- as.data.frame(loading_df_data)
        loading_df_data <- cbind.data.frame(as.numeric(row.names(loading_df_data)),loading_df_data)
        row.names(loading_df_data)<-c()
      }

     if (ncol(undated_df_data) < 2) {
  #      # IF it is the first, then put it in the undated_df_data frame and then remove the NA column
        undated_df_data <- comb.NA(undated_df_data, loading_df_data, fill = NA)
        undated_df_data<-undated_df_data[,-1] # get rid of the NA value column
        #          colnames(undated_df_data)<-c("ring", series)
      } else {
  #      # If it isn't the first undated dated chronology loaded, get the names for the series that have already been loaded
        old_names<-colnames(undated_df_data)
        new_names<-colnames(loading_df_data)[-1] # remove the year column name
  #      # Add the new undated chronology to the data.frame and then recombine the column names
        undated_df_data <- comb.NA(undated_df_data, loading_df_data[,-1], fill = NA)
        colnames(undated_df_data)<-c(old_names,new_names)
      }

      names(undated_df_data) %<>% stringr::str_replace_all("\\s","_") %>% tolower

      colnames(undated_df_data)[1]<-col1

    }

    undated_df_data[,1]<-c(undated_df_data[1,1]:(undated_df_data[1,1] + nrow(undated_df_data) - 1))

    undated_df_data<-name_check(undated_df_data)

    return(undated_df_data)

}


