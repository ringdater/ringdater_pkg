#' Load single chronology as a data frame
#'
#' This function creates a dataframe containing the data from a dated chronology.
#' @keywords RingdateR
#' @param file A  files path as a character string
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplR read.rwl
#' @examples
#' chron_path <- system.file("extdata", "dated_example_excel.xlsx", package="ringdater")
#' load_chron(chron_path)


load_chron<- function(file){

  file_detect<-function(x,n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  # Detect the file format
  ftype<-file_detect(file,3)

  acc_ftypes <- c("rwl", "csv", "lsx", "txt")
  if (!ftype %in% acc_ftypes){
    stop(paste0("Error in load_chron: File type is not supported\n
    Problem file: ",file))
  }

  if(ftype=="rwl"){
   chron_loading_df_data <- readRWL(file)
   chron_loading_df_data <- as.data.frame(chron_loading_df_data)
   chron_loading_df_data <- cbind.data.frame(as.numeric(row.names(chron_loading_df_data)),chron_loading_df_data)
   row.names(chron_loading_df_data)<-c()

  } else if (ftype=="csv"){
   chron_loading_df_data<-read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  } else if (ftype=="lsx"){
   chron_loading_df_data<-as.data.frame(read_excel(file, sheet = 1, na ="NA" ))
  } else if (ftype=="txt"){
   chron_loading_df_data<-read.csv(file, header = TRUE, sep = "/t", stringsAsFactors = FALSE)
  }
  chron_loading_df_data<-name_check(chron_loading_df_data)

  return(chron_loading_df_data)
}
