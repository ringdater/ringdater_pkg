#' Remove series that have been dated from the undated dataframe
#'
#' This function removes data that have been aligned from the originally loaded undated series in the shiny app.
#' @keywords data handling
#' @export
#' @param the.data the dataframe containing the undated series.
#' @param series.id A list of sample ID's (column names) that are to be removed from the.data
#' @examples
#'undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' undated_data <- load_undated(undated_path)
#' samples <- colnames(undated_data )[c(2,3,6)]
#' remove_series(undated_data, samples)
#'
remove_series<-function(the.data, series.id){

  if (class(the.data) != "data.frame"){
    stop("Error in remove_series(). Required data are not a data.frame")
    # Check thre is enough data in the data.frame
  }
  if (ncol(the.data)<=2){
    stop("Error in remove_series(). In sufficient data loaded (loaded data < 2 cols).")
  }
  if (length(series.id)<1){
    stop("Error in remove_series(). No series to remove.")
  }

  for (i in 1:length(series.id)){
      series.2<-series.id[i]

      if(length(which(names(the.data) %in% paste0(parse(text=series.2)))) == 0){
        # warning(paste0("Warning:", series.2, " was not found in the.data so can not be removed"))
        NULL
      } else {
      the.data<- the.data[ , -which(names(the.data) %in% paste0(parse(text=series.2)))]}
    }

    return(the.data)

}
