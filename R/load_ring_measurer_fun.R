#' Load CSV data either from Ring Measurer App or from abother source
#'
#' This function creates a dataframe containing ring width data.
#' @keywords RingdateR
#' @param the_data A dataframe containing ring width measurements
#' @importFrom tools file_path_sans_ext
#' @importFrom magrittr %<>%
#' @param avg_series a boolean to dictate whether replicate series areaveragede or not
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' the_data <- read.csv(undated_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
#' check_load_ringmeasurer_data(undated_path)

check_load_ringmeasurer_data <- function(the_data, avg_series = TRUE){

  # a set of ringmeasurer column names
  key_cols <- c("sample_ID", "x1", "y1", "x2", "y2", "series")  # adjust as needed

  # Return immediately if key columns are missing - not a Ringmasurer file
  if (!all(key_cols %in% colnames(the_data))) {
    return(the_data)
  }

  #If a RM file then process it then return the data
  # get the sample ID
  the_data$sample_ID %<>% stringr::str_replace_all("\\s","_") %>% tolower
  ID <- the_data$sample_ID[1]

  # extract the data from the three series
  series_1 <- the_data[the_data$series == "series_1",]
  series_2 <- the_data[the_data$series == "series_2",]
  series_3 <- the_data[the_data$series == "series_3",]

  # make sure the data are in the right orientation

  series_1  <- series_1[order(series_1$label_text),]
  series_2  <- series_2[order(series_2$label_text),]
  series_3  <- series_3[order(series_3$label_text),]

  # check the lengths of each ofthe datasets
  lens <- c(nrow(series_1), nrow(series_2), nrow(series_3))

  # create a blank variable for the output
  output <- data.frame("year" = 1:max(lens, na.rm = TRUE),
                       "series_1" = rep(NA, max(lens)),
                       "series_2" = rep(NA, max(lens)),
                       "series_3" = rep(NA, max(lens)))


  # # if there is data in the first series put it in the dataframe
  if (lens[1] > 0){
    output$series_1[1:lens[1]] <- series_1$abs_distance
  }

  if (lens[2] > 0){
    output$series_2[1:lens[2]] <- series_2$abs_distance
  }

  if (lens[3] > 0){
    output$series_3[1:lens[3]] <- series_3$abs_distance
  }

  colnames(output) <- c("year", paste0(ID, "_", colnames(output)[2:4]))

  output <- output[,c(1,which(lens>0)+1)]

  # average replicate series
  if (avg_series){
    tmp <- rowMeans(output[,-1], na.rm = TRUE)
    output <- data.frame("years" = 1: length(tmp),
                        ID = tmp)
    colnames(output)[2] = ID
  }

  # return the sorted data for loading into RingdateR
  return(output)
}
