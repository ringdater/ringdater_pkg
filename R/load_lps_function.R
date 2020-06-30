#' Load Image Pro line profile series
#'
#' This function loads lps measurement files created by Image pro as a data.frame
#' @param series The sample ID as a character string.
#' @param path The file path as a character string.
#' @importFrom xml2 as_list
#' @importFrom xml2 read_xml


load_lps<-function(series, path){

  if (class(series) != "character"){
    stop("Error in load_lps(): series not of class character")
  }
  if (class(path) != "character"){
    stop("Error in load_lps(): path not of class character")
  }

    #Read in XML file as a list
    lpsList <- as_list(read_xml(path))

    #The number of lines
    lineCount <- as.numeric(attributes(lpsList$lineprofileengine$lines)$count)

    #the number 1 is the line number
    #the attributes function allows access to the list object attributes
    lines1 <- data.frame(NULL)
    for (i in 1:lineCount){
      #Number of measurements per line
      measureCount <- as.numeric(attributes(lpsList$lineprofileengine$lines[i]$profile$edges$edge$distances$channel$manual)$count)
      distCum <- c()
      meas2 <- c()
      for (j in 1:measureCount){
        #value is the attribute of interest
        meas1 <- as.numeric(attributes(lpsList$lineprofileengine$lines[i]$profile$edges$edge$distances$channel$manual[j]$distance)$value)
        meas2 <- c(meas2, meas1)
      }
      meas2 <- sort(meas2)
      for (j in 1:length(meas2)){
        if (j > 1){
          dist1 <- meas2[j] - meas2[j-1]
          distCum <- c(distCum, dist1)
        }
      }
      #add the line to the data frame
      lines1 <- comb.NA(lines1, distCum, fill = NA)
    }
    #Format the data frame
    lps_loader <- lines1
    lps_loader[,1] <- 1:length(lines1[,1])
    colnames(lps_loader)
    for (i in 1:lineCount){
      colnames(lps_loader)[i+1] <- paste0(series, "_L", i)
    }
    colnames(lps_loader)[1]<-"ring"
    return(lps_loader)

}
