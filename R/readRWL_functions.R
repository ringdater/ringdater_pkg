#' readRWL
#'
#' robust load data from rwl files
#' @keywords rwl load data
#' @importFrom dplR read.rwl
#' @param fName a string containing a file path

readRWL <- function(fName){
  # source("D:/Lab Backup/RingdateR/readRWL/locateID.R")
  # source("D:/Lab Backup/RingdateR/readRWL/readWOheader.R")

  #File extensions:
  ##tridas: .xml
  ##tucson: .rwl
  ##csv: .csv
  ##Heidelberg: .fh
  ##compact: .rwm

  format1 <- c("csv", "tucson", "tucson", "tucson", "tucson",
               "tridas", "heidelberg", "compact")
  ext1 = c(".csv", ".tuc", ".dec", ".crn", ".rwl", ".xml",
           ".fh", ".rwm")

  for (i in 1:length(ext1)){
    if(grepl(ext1[i], tolower(fName), fixed = TRUE)){
      Fformat <- format1[i]
      break
    }
  }


  foo <- function(fName, Fformat){
    suppressWarnings(capture.output(a1 <-
    dplR::read.rwl(fName, format = Fformat)))
    return(a1)
    }


  robustRead <- function(fName, Fformat){
    out <- tryCatch(
      {
        rwlFile <- foo(fName, Fformat)
      },
      error = function(e){
        if(Fformat != "tucson"){
          stop("Sorry, can't read this file. We recommend csv or tucson format.", "\n")
        }
        startPT <- locateID(fName)
        rwlFile <- readWOheader(fName, startPT)
        return(rwlFile)
      }
    )

    return(out)
  }

  rwlOut <- robustRead(fName, Fformat)
  return(rwlOut)
}

#' readWOheader
#'
#' find and remove header from rwl files
#' @keywords rwl load data
#' @param fName a string containing a file path
#' @param startPT a string containing the start of a sample ID
readWOheader <- function(fName, startPT){
  # import the data in a single column. Use the # as the seperator to achive this
  the_data <- as.data.frame(read.table(fName, sep = "#"))

  # find the first actual row of data. I've done this a bit badly, but it seems to work
  found <- 0
  for (i in 1:nrow(the_data)){
    if(grepl(startPT, tolower(the_data[i,1]), fixed = TRUE)){
      found <- i
      break
    }
  }

  # remove all the rows that contain the header
  # note this will still leave a row with junk
  # at the start which still needs to be removed as
  # there is data at the end of this row
  the_data <- as.data.frame(the_data[-c(1:(found-1)),])

  # get rid of the junk
  end <- stringr::str_locate(tolower(the_data[1,1]), startPT)[2]
  the_data[1,1] <- stringr::str_sub(string = the_data[1,1], start = (end - nchar(startPT) + 1), end = stringr::str_length(the_data[1,1]))

  # save the cleaned data to a csv file and then reload it as an rwl
  # this is where we need to read and write to a tmp file if possible.
  tFile <- tempfile(pattern = "", fileext = ".csv")
  write.table(x = the_data,sep = ",", file = tFile, row.names = FALSE, col.names = FALSE, quote = FALSE)
  suppressWarnings(capture.output(new_data <- dplR::read.rwl(tFile, format = "tucson")))
  unlink(tFile)
  return(new_data)
}

#' locateID
#'
#' find sample IDs within RWL files
#' @keywords rwl load data
#' @param fName a string containing a file path
locateID <- function(fName){

  cat("Attempting to resolve problem with tucson file header.", "\n")

  #Helper functions
  implode <- function(..., sep='') {
    paste(..., collapse=sep)
  }

  getMode <- function(x){
    findMode <- rep(NA, length(unique(x)))
    for (i in 1:length(unique(x))){
      findMode[i] <- sum(x == unique(x)[i])
    }
    mode1 <- unique(x)[findMode == max(findMode)]
    return(mode1)
  }


  #Read the first word of each line
  sampName1 <- rep(NA, length(suppressWarnings(readLines(fName))))
  sampLen1 <- rep(NA, length(suppressWarnings(readLines(fName))))

  for (i in 1:length(suppressWarnings(readLines(fName)))){
    sampName1[i] <- tolower(gsub(" .*", '', readLines(fName)[i]))
    sampLen1[i] <- nchar(sampName1[i])
  }


  #Find the most common length - length of sample name
  nameLen <- getMode(sampLen1)


  #find the shared site tag (first 2-3 letters)
  nameTrunc <- sampName1[sampLen1 == nameLen]

  let1 <- rep(NA, 6)
  for (i in 1:6){
    let1[i] <- getMode(substr(nameTrunc,i,i))

    #All of the sample names should contain this character
    if(!(sum(let1[i] == substr(nameTrunc,i,i)) == length(nameTrunc))){
      let1[i] <- NA
      break
    }


  }
  #cat("Site ID preview: ", let1, "\n")
  let1 <- let1[complete.cases(let1)]
  siteID <- implode(let1)

  #cat("Site ID: ", siteID, "\n")
  #cat("Sample ID length: ", nameLen, "\n")

  #Search the first several lines to locate the first sample name
  stopHere <- 0
  countLines <- 1
  while(stopHere == 0){
    a <- suppressWarnings(readLines(fName)[countLines])
    words1 <- tolower(unlist(strsplit(a, " ")))
    #cat("Print line: ", words1, "\n")
    if (anyNA(words1)){
      next
    }
    for (i in 1:length(words1)){
      if(is.null(words1[i])){
        break
      }else if (anyNA(words1[i])){
        break
      }
      #cat("Length of word: ", nchar(words1[i]), "\n")
      #cat("First 3 letters of word: ", substr(words1[i], 1, length(let1)), "\n")
      if((nchar(words1[i]) == nameLen) && (substr(words1[i], 1, length(let1)) == siteID)){
        # cat("First sample ID located at line: ", countLines, ", word ", i, "\n", words1[i], "\n")
        stopHere <- 1
        break
      }
    }
    countLines <- countLines + 1
  }

  return(words1[i])
}
