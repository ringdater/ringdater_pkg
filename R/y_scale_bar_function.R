#' Generate a set of breaks for a y-axis
#'
#' his function sets the breaks for the y-axis in the RingdateR plots
#' @keywords scale_x_Axis
#' @param y.min a numeric integer
#' @param y.max a numeric integer#'

y.scale.bar<-function(y.min, y.max){

  if (class(y.min) != "numeric"){
    stop("Error in y.scale.bar: y.min is not a numeric integer")
  }
  if ( class(y.max) != "numeric"){
    stop("Error in y.scale.bar: y.miax is not a numeric integer")
  }
  if(y.max<=y.min){
    stop("Error in y.scale.bar: y.max must be great than y.min")
  }

    if(y.max-y.min>1000){
      breaks<-seq(y.min, y.max, by = 100)
    }  else if(y.max-y.min>500){
       breaks<-seq(y.min, y.max, by = 50)
       } else if(y.max-y.min>250){
         breaks<-seq(y.min, y.max, by = 20)
       } else if (y.max-y.min>50){
         breaks<-seq(y.min, y.max, by = 10)
       } else if (y.max-y.min>20){
         breaks<-seq(y.min, y.max, by = 5)
       } else{breaks<-seq(y.min, y.max, by = 2) }

    return(breaks)

}

