#' Generate a set of breaks for an x-axis
#'
#' This function sets the breaks for the x-axis in the RingdateR plots
#' @keywords scale_x_Axis
#' @param x.min a numeric integer
#' @param x.max a numeric integer
#' @export

x.scale.bar<-function(x.min,x.max){
  run<-TRUE
  if (class(x.min) != "numeric"){
    NULL
  } else if ( class(x.max) != "numeric"){
    NULL
  } else {
    if(((x.max-x.min)>1000) == TRUE){
       breaks<-seq(x.min, x.max, by = 100) # 100
    }  else if(x.max-x.min>500){
       breaks<-seq(x.min, x.max, by = 50)  # 50
       } else if(x.max-x.min>100){
         breaks<-seq(x.min, x.max, by = 20)
       } else if (x.max-x.min>50){
         breaks<-seq(x.min, x.max, by = 10)
       } else if (x.max-x.min>20){
         breaks<-seq(x.min, x.max, by = 5)
       } else{breaks<-seq(x.min, x.max, by = 2)}

    return(breaks)
  }
}
