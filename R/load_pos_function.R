#' Load a pos file as a dataframe
#'
#' This function reads pos formated files created by Image Pro and outputs a data.frame
#' @keywords pos, image pro
#' @param file_path A character string containing the file path to be loaded.


load_pos<-function(file_path){

  if (class(file_path) != "character"){
    stop("Error in load_lps(): file_path not of class character")
  }

    #Read in the POS data
    POStest<-read.table(file_path, sep = "/")
    #initiate blank DF for formatted POS data
    POSdata <- data.frame()
    naFill <- c(NA, NA)
    gapFill <- c(NA, 1)
    gapCount <- 0
    #initiate blank vector for measurements
    ringWidths <- c()
    lastGap <- FALSE
    for (i in 2:dim(POStest)[1]){
      #initiate switches for special cases
      gapSwitch <- FALSE
      latSwitch <- FALSE
      #grab current line data and initiate DF to collect formatted line data
      currentLine <- as.character(POStest[i,1])
      spl2 <- data.frame()
      #if the line is a gap measurement pick it out by the 'D' at the begining
      if(gregexpr('D', currentLine)[[1]][1] == 1){
        gapSwitch <- TRUE
        #remove the 'D' after noting that the line contains a gap measurement
        currentLine <- gsub("D", "", currentLine)
      }
      if (gregexpr('  ', currentLine)[[1]] != -1){
        latSwitch <- TRUE
        currentLine <-strsplit(currentLine, "  ")
        for (j in 1:length(currentLine[[1]])){
          spl2 <- strsplit(currentLine[[1]], ",")
          spl2 <- as.data.frame(t(c(as.numeric(spl2[[1]]), as.numeric(spl2[[2]]))))
        }
      }else{
        for (j in 1:length(currentLine[[1]])){
          spl2 <- as.data.frame(t(c(naFill, as.numeric(strsplit(currentLine[[1]][j], ",")[[1]]))))
          if(gapSwitch == TRUE){
            spl2 <- as.data.frame(t(c(gapFill, as.numeric(strsplit(currentLine[[1]][j], ",")[[1]]))))
          }
        }
      }
      colnames(spl2) <- c("A", "B", "C", "D")
      POSdata <- rbind.data.frame(POSdata, spl2)
      #######################################################################################################
      #Advance through the organized DF measuring the ring widths
      #######################################################################################################
      #start after we have 2 lines read
      k=i-1
      if (i>=3){
        #set typical x and x coordinates
        x1 <- POSdata[k,3]
        y1 <- POSdata[k,4]
        x2 <- POSdata[k-1,3]
        y2 <- POSdata[k-1,4]
        #If the previous measurement was a gap
        if (lastGap == TRUE){
          if (latSwitch == TRUE){
            x1 <- POSdata[k,1]
            y1 <- POSdata[k,2]
            x2 <- POSdata[k-3,3]
            y2 <- POSdata[k-3,4]
          }else{
            x2 <- POSdata[k-3,3]
            y2 <- POSdata[k-3,4]
          }
          dist1 <- sqrt((x1-x2)^2 + (y1-y2)^2)
          dist1 <- dist1 - distGap
          distGap <- 0
          #Add ring width to measurements list
          ringWidths <- c(ringWidths, dist1)
          lastGap <- FALSE
        }else if (latSwitch == TRUE){
          #if we are on a lateral jump
          x1 <- POSdata[k,1]
          y1 <- POSdata[k,2]
          #measure ring width
          dist1 <- sqrt((x1-x2)^2 + (y1-y2)^2)
          #Add ring width to measurements list
          ringWidths <- c(ringWidths, dist1)
        }else if (gapSwitch == TRUE){
          #if we are on a gap
          gapCount <- gapCount + 1
          #if we are on the far side of a gap
          if(gapCount == 2){
            lastGap <- TRUE
            gapCount <- 0
            distGap <- sqrt((x1-x2)^2 + (y1-y2)^2)
          }else if (gapCount == 1){
            #do nothing
          }
        }else{
          #measure ring width
          dist1 <- sqrt((x1-x2)^2 + (y1-y2)^2)
          #Add ring width to measurements list
          ringWidths <- c(ringWidths, dist1)
        }
      }
    }
    ring.widths <- cbind(1:length(ringWidths), rev(ringWidths))
    return(ring.widths)

}
