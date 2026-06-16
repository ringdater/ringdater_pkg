run_chrono_checker<-function(){
  options(shiny.maxRequestSize = 50*1024^2)
  registerDoParallel(cores=2)
  runApp(list(ui = chrono_check_ui, server = chrono_check_server), launch.browser = TRUE)
}
