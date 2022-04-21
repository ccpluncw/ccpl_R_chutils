#' A function to filter data based on the number of elements.
#'
#' This function filters the data, it can be grouped using a grouping variable.  Basically, it removes all the rows that have fewer than lowNthreshold observations. It returns a list that contains a dataframe of the removed data, a dataframe of the the kept data, the number of datapoints removed, and the percentage of points removed.
#' @param data a dataframe containing the data.
#' @param dataCol a string that specifies the name of the column in "data" on which the thresholds are to act.
#' @param grpCol a string or vector of strings that specify the names of the column in "data" that act as grouping variables.
#' @param lowNthreshold An integer above 0 that specifies the minimum number of observations that the condition must have to be kept in the dataset. DEFAULT = 0
#' @keywords between data filter group N length
#' @return a list containing: (datRemoved) a dataframe with the removed data as grouped by grpCol; (datKept) a dataframe with the kept data as grouped by grpCol; (numRemoved) the number of datapoints removed; (pRemoved) the proportion of datapoints removed; (datKeptRaw) a dataframe with the kept data in the raw format (ungrouped);.
#' @export
#' @examples ch.filterGrpByN (data=myDataFrame, "RT", "overlapRound", lowNthreshold=10)

ch.filterGrpByN <- function(data, dataCol, grpCol = NULL, lowNthreshold=0) {

    if(lowNthreshold < 0) {
    stop("both your lowNthreshold must be greater than or equal to 0")
  }

  if(is.null(grpCol)) {
    data.1 <- data
    #the code below gets the length without counting NAs
    data.1$NxxTmp <- sum(!is.na(data[[dataCol]]))
  } else {
    sumDat <- data.frame(data %>% dplyr::group_by_at(grpCol) %>% dplyr::summarise( NxxTmp = sum(!is.na(eval(parse(text=dataCol)))) ) )

    data.1 <- merge(data,sumDat)
  }

  datKept <- data.1[data.1$NxxTmp >= lowNthreshold,]
  datRemoved <-data.1[data.1$NxxTmp < lowNthreshold,]

  if(length(datRemoved[[dataCol]]) == 0) {
    numRemoved <- 0
    pRemoved <- 0
  } else {
    numRemoved <- length(datRemoved[[dataCol]])
    pRemoved <- numRemoved/length(data[[dataCol]])
  }

  datKept <- subset(datKept, select = -c(NxxTmp) )

  outList <- list(datKept = datKept, datRemoved = datRemoved, numRemoved = numRemoved, pRemoved = pRemoved)

  return(outList)
}
