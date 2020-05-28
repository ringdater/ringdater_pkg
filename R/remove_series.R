#' Remove series that have been dated from the undated dataframe
#'
#' This function removes data that have been aligned from the orignially loaded undated series in the shiny app.
#' @keywords data handling
#' @export
#' @param the.data the dataframe containing the undated series.
#' @param series.id A list of sample ID's (column names) that are to be removed from the.data

remove_series<-function(the.data, series.id){

  run <- TRUE

  if (class(the.data) != "data.frame"){
    warning("Error in remove_series(). Required data are not a data.frame")
    run <- FALSE
    # Check thre is enough data in the data.frame
  } else if (ncol(the.data)<=2){
    warning("Error in remove_series(). Loaded data contains no data.")
    run <- FALSE
  }

  if (length(series.id)<1){
    warning("Error in remove_series(). No series to remove.")
    run <- FALSE
  }
  if (run){
    for (i in 1:length(series.id)){
      series.2<-series.id[i]

      if(length(which(names(the.data) %in% paste0(parse(text=series.2)))) == 0){
        NULL
      } else {
      the.data<- the.data[ , -which(names(the.data) %in% paste0(parse(text=series.2)))]}
    }

    return(the.data)
  }
}
