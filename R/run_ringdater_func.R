#' Run RingdateR as a shiny app
#'
#' This function runs RingdateR as a shiny app
#' @keywords Shiny crossdating app
#' @importFrom doParallel registerDoParallel
#' @export

run_ringdater<-function(){
  options(shiny.maxRequestSize = 50*1024^2)
  registerDoParallel(cores=2)
  shinyApp(ui = ui, server = RingServer)
}
