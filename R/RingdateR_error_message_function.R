#' Display error messages in a plot
#'
#' This function creates error messages to be displayed in place of a plot in the shiny app
#' @keywords errors
#' @param plot.err TRUE/FALSE
#' @param message A string of characters to convey an error message.
#' @export
#' @examples
#' RingdateR_error_message(message = "There has been an error")

RingdateR_error_message<-function(message="Can't display plot", plot.err = TRUE){

  if(class(plot.err)!= "logical"){
    stop("Warning RingdateR_error_message: plot.err not logical (TRUE?FLASE)")
  }
  if(class(message)!= "character"){
    stop("Warning RingdateR_error_message: message not a character string")
  }

  the.data<-data.frame(c(message), c(1), c(1))
  colnames(the.data) <-c("Error message", "x", "y")

  plot1<-ggplot()+
    geom_text(data = the.data, aes(x=the.data[1,2], y=the.data[1,3], label =the.data[1,1]), colour = "red", size = 10) +
    theme(text = element_blank(),
          line = element_blank(),
          panel.background = element_blank())

  if (plot.err==T){
  return(plot1)
    } else { return(the.data[1,1])}
}


