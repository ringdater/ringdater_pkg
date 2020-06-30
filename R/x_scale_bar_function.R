#' Generate a set of breaks for an x-axis
#'
#' This function sets the breaks for the x-axis in the RingdateR plots
#' @keywords scale_x_Axis
#' @param x.min a numeric integer
#' @param x.max a numeric integer

x.scale.bar<-function(x.min,x.max){

  if (class(x.min) != "numeric"){
    stop("Error in x.scale.bar: x.min is not a numeric integer")
  }
  if ( class(x.max) != "numeric"){
    stop("Error in x.scale.bar: x.miax is not a numeric integer")
  }
  if(x.max<=x.min){
    stop("Errpr in x.scale.bar: x.max must be greater than x.min")
  }

  if((x.max-x.min)>1000){
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
