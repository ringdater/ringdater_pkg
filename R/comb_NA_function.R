#' combine data.frames
#'
#' Combines vector or data.frame with a dataframe of a different length/nrows
#' @keywords combine data.frames
#' @param ... Either a dataframe and a vector or two data frames to be combined.
#' @param fill Either NA or a character string to be used to extend columns
#' @export
#' @examples
#' df1<-data.frame(a=1:10, b=2:11)
#' df2<-data.frame(c=5:10, d=6:11)
#' comb.NA(df1,df2)

comb.NA <- function (..., fill = NA)
{
  DFx <- list(...)
  for (i in 1:length(DFx)){
    if (is.null(dim(DFx[[i]]))){
      DFx[[i]] <- as.data.frame(DFx[[i]])
      next()
    }
    if (dim(DFx[[i]])[1] == 0){
      DFx[[i]] <- data.frame(rep(NA, 1))
    }
  }
  lengthsDF <- as.numeric(lapply(DFx, vertLen))
  maxLen <- max(lengthsDF)
  newDF <- rep(NA, maxLen)
  insideDF <- NULL
  for(i in 1:length(DFx)){
    insideDF <- NULL
    if(lengthsDF[i] < maxLen){
      padLen <- maxLen - lengthsDF[i]
      padding <- data.frame(matrix(nrow =  padLen, ncol = dim(DFx[[i]])[2], data = NA))
      names(padding) <- names(DFx[[i]])
      insideDF <- rbind(DFx[[i]], padding)
    }else{
      insideDF <- DFx[[i]]}
    newDF <- cbind(newDF, insideDF)
  }
  newDF <- newDF[,-1]
  return(newDF)
}

#' Calculate the length of a dataframe
#'
#' Calculatesthe length of a dataframe
#' @keywords combine data.frames
#' @param dfIn A dataframe that is being combined
#' @export

vertLen <-  function(dfIn){
  if(is.null(dim(dfIn))){
    vertical <- length(dfIn)
  }else{
    vertical <- length(dfIn[,1])
  }
  return(vertical)
}

