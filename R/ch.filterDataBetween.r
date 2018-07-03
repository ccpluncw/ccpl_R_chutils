#' A function to filter data between two thresholds
#'
#' This function filters the data between a low threshold and/or a high threshold. It returns a list that contains a dataframe of the removed data, a dataframe of the the kept data, the number of datapoints removed, and the percentage of points removed.
#' @param data a dataframe containing the data.
#' @param dataCol a string that specifies the name of the column in "data" on which the thresholds are to act.
#' @param lowThresh the lower threshold that the data must be above to be kept.
#' @param highThresh the higher threshold that the data must be below to be kept.
#' @keywords between data filter
#' @return a list containing: (datRemoved) a dataframe with the removed data; (datKept) a dataframe with the kept data; (numRemoved) the number of datapoints removed; (pRemoved) the proportion of datapoints removed.
#' @export
#' @examples ch.filterDataBetween (data=myDataFrame, "RT", 200, 5000)

ch.filterDataBetween <- function(data, dataCol, lowThresh = NULL, highThresh = NULL) {
  if(is.null(highThresh) & is.null(lowThresh)){
    stop("at least one Threshold must be entered: lowThresh and/or highThresh ")
  }
  if(is.null(lowThresh)){
    datRemoved <-data[data[[dataCol]] >= highThresh,]
    datKept <- data[(data[[dataCol]] < highThresh),]
  } else {
    if(is.null(highThresh)) {
      datRemoved <- data[data[[dataCol]] <= lowThresh,]
      datKept <- data[data[[dataCol]] > lowThresh,]
    } else {
      if(lowThresh >= highThresh) {
        stop("lowThresh must be less than highThresh")
      }
      datRemoved <-data[data[[dataCol]] >= highThresh |data[[dataCol]] <= lowThresh,]
      datKept <- data[(data[[dataCol]] < highThresh & data[[dataCol]] > lowThresh),]
    }
  }
  numRemoved <- length(datRemoved[[dataCol]])
  pRemoved <- numRemoved/length(data[[dataCol]])

  outList <- list(datKept = datKept, datRemoved = datRemoved, numRemoved = numRemoved, pRemoved = pRemoved)

  return(outList)
}

#' A function to filter data between two thresholds grouped by a single column
#'
#' This function filters the data, based on a grouping variable, between a low threshold and/or a high threshold. It returns a list that contains a dataframe of the removed data, a dataframe of the the kept data, the number of datapoints removed, and the percentage of points removed.
#' @param data a dataframe containing the data.
#' @param dataCol a string that specifies the name of the column in "data" on which the thresholds are to act.
#' @param grpCol a string that specifies the name of the column in "data" that acts as a grouping variable.
#' @param lowThresh the lower threshold that the data must be above to be kept.
#' @param highThresh the higher threshold that the data must be below to be kept.
#' @param FUN the function that specifies the how to combine dataCol by grpCol (e.g., mean).
#' @keywords between data filter group
#' @return a list containing: (datRemoved) a dataframe with the removed data as grouped by grpCol; (datKept) a dataframe with the kept data as grouped by grpCol; (numRemoved) the number of datapoints removed; (pRemoved) the proportion of datapoints removed; (datKeptRaw) a dataframe with the kept data in the raw format (ungrouped);.
#' @export
#' @examples ch.filterGrpBtwn (data=myDataFrame, "RT", "sn", 200, 5000, mean)

ch.filterGrpBtwn <- function(data, dataCol, grpCol, lowThresh=NULL, highThresh=NULL, FUN) {
  library(chutils)

  if(length(grpCol)!=1) {
    stop("grpCol required to be of length 1")
  }
  if(is.null(highThresh) & is.null(lowThresh)){
    stop("at least one Threshold must be entered: lowThresh and/or highThresh ")
  }

  sumDat <- ch.summariseBy(data, grpCol, dataCol, "out", FUN)
  outList <- ch.filterDataBetween(sumDat, "out", lowThresh, highThresh)

  datKeptRaw <- data[!(data[[grpCol]] %in% outList$datRemoved[[grpCol]]), ]

  outList[["datKeptRaw"]] <- datKeptRaw

  return(outList)
}
