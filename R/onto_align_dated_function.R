#' Ontogenetically align series that are currently date aligned
#'
#' Align samples that have been crossdated using a target sample.
#' @keywords Crossdate, ontogenetic, align
#' @param df A dataframe containing the date aligned series to be aligned.
#' @export

onto_align_dated <- function(df){
  #set up the output data frame using the first sample
  onto <- data.frame(df[!is.na(df[,2]),2])
  colnames(onto) <- colnames(df)[2]

  for (i in 3:ncol(df)){
    tmp <- data.frame(df[!is.na(df[,i]),i])
    colnames(tmp) <- colnames(df)[i]
    onto <- comb.NA(onto, tmp)
  }
  ring <- 1:nrow(onto)

  onto <- cbind(ring, onto)
  return(onto)
}


