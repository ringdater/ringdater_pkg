#' Load CSV data either from Ring Measurer App or from abother source
#'
#' This function creates a dataframe containing ring width data.
#' @keywords RingdateR
#' @param the_data A dataframe containing ring width measurements
#' @param file A string holding the path to the data. used to get the sample ID that should be the file name
#' @importFrom tools file_path_sans_ext
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' the_data <- read.csv(undated_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
#' check_load_ringmeasurer_data(undated_path)

check_load_ringmeasurer_data <- function(the_data, avg_series = TRUE){

   RGMR_names <- c("sample_ID","x1",	"y1",	"x2",	"y2",	"mode",	"series",	"ob_type",	"ind",	"text",	"px_distance",
                  "abs_distance",	"calibration",	"calibrated",	"label_text",	"year",	"dated",	"col")

  if (!setequal(colnames(the_data), RGMR_names)){return(the_data)
  } else {

    # get the sample ID
    the_data$sample_ID %<>% stringr::str_replace_all("\\s","_") %>% tolower
    ID <- the_data$sample_ID[1]

    # extract the data from the three series
    series_1 <- the_data[the_data$series == "series_1",]
    series_2 <- the_data[the_data$series == "series_2",]
    series_3 <- the_data[the_data$series == "series_3",]


    # make sure the data are in the right orientation
    series_1  <- series_1[order(series_1$year),]
    series_2  <- series_2[order(series_2$year),]
    series_3  <- series_3[order(series_3$year),]

    # check the lengths of each ofthe datasets
    lens <- c(nrow(series_1), nrow(series_2), nrow(series_3))

    # create a blank variable for the output
    output <- NULL

    # will be the list of sample IDs
    out_ids = c("year")

    # # if there is data in the first series put it in the dataframe
    if (lens[1] > 0){
      output <- data.frame(series_1$year,
                           series_1$abs_distance)
      out_ids = c(out_ids, paste0(ID, "_series_1" ))
    }

    # if there is data in the second series put it in the dataframe
    if (lens[2] > 0 && is.null(output)){
      output <- data.frame(series_2$year,
                           series_2$abs_distance)
      out_ids = c(out_ids, paste0(ID, "_series_2" ))
    } else if (lens[2] > 0 && !is.null(output)){

      output <- cbind(output, series_2$abs_distance)
      out_ids = c(out_ids, paste0(ID, "_series_2" ))
    }

    # if there is data in the third series put it in the dataframe
    if (lens[3] > 0 && is.null(output)){
      output <- data.frame(series_3$year,
                           series_3$abs_distance)
      out_ids = c(out_ids, paste0(ID, "_series_3" ))
    } else if (lens[3] > 0 && !is.null(output)) {
      output <- cbind(output, series_3$abs_distance)
      out_ids = c(out_ids, paste0(ID, "_series_3" ))
    }

    # give the series the proper sample ID based on the file name
    colnames(output) <- out_ids

    # average replicate series

    if (avg_series){
      tmp_years <- output[,1]
      tmp_data <- as.data.frame(output[,-1])
      if (ncol(tmp_data) > 1){
        tmp_means <- rowMeans(tmp_data, na.rm=TRUE)
      } else {
        tmp_means <- tmp_data
      }
      output <- data.frame(tmp_years, tmp_means)
      colnames(output) <- c("year", ID)
    }

    # return the sorted data for loading into RingdateR
    return(output)
  }
}
