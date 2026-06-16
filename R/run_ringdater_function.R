#' Run RingdateR as a shiny app
#'
#' This function runs RingdateR as a shiny app
#' @keywords Shiny crossdating app
#' @importFrom doParallel registerDoParallel
#' @export
#' @examples
#' # To launch RingdateR as a shiny app simply run:
#' # run_ringdater()

run_ringdater<-function(){
  options(shiny.maxRequestSize = 50*1024^2)
  registerDoParallel(cores=2)
  runApp(list(ui = ui, server = RingServer), launch.browser = TRUE)
}
