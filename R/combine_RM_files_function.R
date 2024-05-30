#' Loada bunch of Ring MEasurer measurement files in to a single CSV file
#'
#' This function creates a dataframe containing ring width data.
#' @keywords RingdateR Ring Measurer
#' @importFrom magrittr %<>%
#' @param path A string containing the path for a directory containing measurement files - these can be in subdirectoriers
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' the_data <- read.csv(undated_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
#' check_load_ringmeasurer_data(undated_path)
#'

combine_RM_files <- function(path){
  csv_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  output <- NULL
  errors <- c()

  for (i in 1:length(csv_files)){
    tmp <- read.csv(csv_files[i], header = TRUE)
    tmp <- check_load_ringmeasurer_data(tmp)
    if ((class(tmp) != "data.frame") && (tmp == "error")){
      errors <- c(errors, csv_files[i])
    } else {
      if (is.null(output)){
        output <- tmp
      } else {
        output_len <- nrow(output)
        tmp_len <- nrow(tmp)
        dif <- output_len - tmp_len # neg = tmp = longer

        if (dif < 0){
          new_years <- 1:tmp_len
          na_ser <- rep(NA, abs(dif))
          new_output <- data.frame(years = new_years)
          for (j in 2:ncol(output)){
            output_RGI <- c(output[,j], na_ser)
            new_output <- cbind(new_output, output_RGI)
          }
          colnames(new_output) <- colnames(output)
          output <- cbind(new_output, tmp[,-1])
          colnames(output) <- c(colnames(new_output), colnames(tmp[-1]))
        } else if (dif > 0){
          output_names <- colnames(output)
          new_tmp <- data.frame(years <- output[,1])
          for (j in 2:ncol(tmp)){
            na_ser <- rep(NA, abs(dif))
            tmp_RGI <- c(tmp[,j], na_ser)
            new_tmp <- cbind(new_tmp, tmp_RGI)
            output <- cbind(output, tmp_RGI)
          }
          colnames(output) <- c(output_names, colnames(tmp)[-1])
        } else {
          output_names <- colnames(output)
          output <- cbind(output, tmp[,-1])
          colnames(output) <- c(output_names, colnames(tmp[-1]))
        }
      }
    }
  }
  data <- list(rwi = output,
               errors = errors)
  return(data)
}
