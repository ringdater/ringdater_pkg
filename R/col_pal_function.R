#' Set colours for RingdateR plots
#'
#' Sets the colour pallate for the heatmaps in RingdateR
#'
#' 1 = blue to white to red;
#' 2 = white to red;
#' 3 = white to blue;
#' 4 = white to black.
#'
#' @keywords Colours
#' @param colour_scale A numeric from 1 to 4. 1 = blue to white to red;
#' 2 = white to red; 3 = white to blue; 4 = white to black.
#' @export
#' @examples
#' col_pal(1)
col_pal <- function(colour_scale = 1){

  # Check the input is the correct class and in the correct range
  if (colour_scale %in% c(1:4) == FALSE){
    stop("Error in col_pal(). colour_scale must be a numeric from 1 to 4.")
  }

  if(colour_scale == 1){col_scale <- c("#4575b4","#e0f3f8","#d73027")
    } else if(colour_scale == 2){col_scale <- c("#ffffff", "#ffffff", "#ca0020")
    } else if(colour_scale == 3){col_scale <- c("#ffffff","#ffffff","#ffffff", "#0571b0","#00216d")
    } else if(colour_scale == 4){col_scale <- c("#ffffff","#ffffff", "#000000")
    }

  return(col_scale)

}
