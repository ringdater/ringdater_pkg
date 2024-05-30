#' Align samples in time for loading
#'
#' Align samples in time for the loading processes
#' @keywords aligning dataframes in time
#' @param undated_df_data A dataftrame containing the previously loaded data. Should be at least two columns, first column the year values and rest growth data
#' @param loading_df_data A dataftrame containing the new data to load. Should be at least two columns, first column the year values and rest growth data
#' @export
#' @examples
#'
#' undated_df_data  <- data.frame(years = 10:50, sample_a = runif(41))
#' loading_df_data <- data.frame(years = 25:75, sample_b = runif(51))
#' align_undated_load(undated_df_data, loading_df_data)

align_undated_load <- function(undated_df_data, loading_df_data){

  min_year <- min(min(undated_df_data[,1], na.rm = TRUE), min(loading_df_data[,1], na.rm = TRUE))
  max_year <- max(max(undated_df_data[,1], na.rm = TRUE), max(loading_df_data[,1], na.rm = TRUE))

  new_years <- min_year:max_year
  aligned_data <- data.frame(years = min_year:max_year)

  output_names <- c(colnames(undated_df_data), colnames(loading_df_data)[-1])

  i <- 2
  for (i in 2:ncol(undated_df_data)){

    tmp <- undated_df_data[,c(1,i)]
    tmp <- subset(tmp, complete.cases(tmp))

    min_date <- min(tmp[,1])
    dif <- abs(min_date - min_year)

    tmp <- tmp[,2]

    if (dif>=1){
      NA_ser<-rep(NA,dif)
      tmp<-c(NA_ser,tmp)
    }
    aligned_data<-comb.NA(aligned_data,tmp, fill = NA)
    tmp<-NULL
  }

  for (i in 2:ncol(loading_df_data)){

    tmp <- loading_df_data[,c(1,i)]
    tmp <- subset(tmp, complete.cases(tmp))

    min_date <- min(tmp[,1])
    dif <- abs(min_date - min_year)

    tmp <- tmp[,2]

    if (dif>=1){
      NA_ser<-rep(NA,dif)
      tmp<-c(NA_ser,tmp)
    }
    aligned_data<-comb.NA(aligned_data,tmp, fill = NA)
    tmp<-NULL
  }

  colnames(aligned_data) <- output_names
  return(aligned_data)
}

