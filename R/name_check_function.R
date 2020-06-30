#' Check the names of samples loaded into RingdateR
#'
#' This function checks the column names of samples loaded into RingdateR. IT will automatically change them
#' to make them suitable for use in R and other functions in RingdateR
#' @param the_data A dataframe to check the colnames.
#' @export
#' @examples
#' undated_path <- system.file("extdata", "undated_example.csv", package="ringdater")
#' undated_data <- load_undated(undated_path)
#' name_check(undated_data)

name_check<-function(the_data){

  if (class(the_data) != "data.frame"){
    stop("Error in name_check: the_data is not a valid data.frame")
  }

  # Extract the old names
  old_names<- colnames(the_data)

  # Modify names to make them suitable as variable ID's
  new_names<-make.names(names = colnames(the_data), unique = TRUE)
  new_names<-gsub("\\.","_", new_names)

  # Make them look pretty
  for ( i in 1: length(old_names)){
    if ((substr(old_names[i], start = 1, stop = 1) == substr(new_names[i], start = 1, stop = 1)) == FALSE){
      new_names[i]<-paste0("ID_",substr(new_names[i], start = 2, stop = nchar(old_names[i])+1))
    }
    if (substr(old_names[i], start = 1, stop = 1) == "x"){
      new_names[i]<-paste0("ID_",substr(new_names[i], start = 2, stop = nchar(old_names[i])+1))
    }
  }

  # append them back to the dataframe
  colnames(the_data) <- new_names

  return(the_data)
}
