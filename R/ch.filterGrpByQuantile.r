
#' A function to filter data based on the quantile value.
#'
#' This function filters the data, it can be grouped using a grouping variable, between a low quantile threshold and/or a high quantile threshold. It returns a list that contains a dataframe of the removed data, a dataframe of the the kept data, the number of datapoints removed, and the percentage of points removed.
#' @param data a dataframe containing the data.
#' @param dataCol a string that specifies the name of the column in "data" on which the thresholds are to act.
#' @param grpCol a string or vector of strings that specify the names of the column in "data" that act as grouping variables.
#' @param lowQuantileThreshold A decimal between 0 and 1 that specifies the low quantile threshold, that the data must be above to be kept.
#' @param highQuantileThreshold A decimal between 0 and 1 that specifies the high quantile threshold, that the data must be below to be kept.
#' @keywords between data filter group quantile
#' @return a list containing: (datRemoved) a dataframe with the removed data as grouped by grpCol; (datKept) a dataframe with the kept data as grouped by grpCol; (numRemoved) the number of datapoints removed; (pRemoved) the proportion of datapoints removed; (datKeptRaw) a dataframe with the kept data in the raw format (ungrouped);.
#' @export
#' @examples ch.filterGrpByQuantile (data=myDataFrame, "RT", "sn", lowQuantileThreshold=0.05, highQuantileThreshold=0.95)

ch.filterGrpByQuantile <- function(data, dataCol, grpCol = NULL, lowQuantileThreshold=0, highQuantileThreshold=1) {

  if(lowQuantileThreshold >= highQuantileThreshold) {
    stop("lowQuantileThreshold must be less than highQuantileThreshold")
  }
  if(lowQuantileThreshold > 1 | highQuantileThreshold > 1 ) {
    stop("both your thresholds must be between 0 and 1")
  }
  if(lowQuantileThreshold < 0 | highQuantileThreshold < 0 ) {
    stop("both your thresholds must be between 0 and 1")
  }

  if(is.null(grpCol)) {
    data.1 <- data
    data.1$lowQ <- quantile(data[[dataCol]], lowQuantileThreshold, na.rm = T)
    data.1$highQ <- quantile(data[[dataCol]], highQuantileThreshold, na.rm = T)
  } else {
    sumDat <- data.frame(data %>% dplyr::group_by_at(grpCol) %>% dplyr::summarise( lowQ = quantile(eval(parse(text=dataCol)), lowQuantileThreshold, na.rm = T), highQ = quantile(eval(parse(text=dataCol)), highQuantileThreshold, na.rm = T)))

    data.1 <- merge(data,sumDat)
  }

  datKept <- data.1[data.1[[dataCol]] >= data.1$lowQ & data.1[[dataCol]] <= data.1$highQ,]
  datRemoved <-data.1[data.1[[dataCol]] > data.1$highQ | data.1[[dataCol]] < data.1$lowQ,]

  if(length(datRemoved[[dataCol]]) == 0) {
    numRemoved <- 0
    pRemoved <- 0
  } else {
    numRemoved <- length(datRemoved[[dataCol]])
    pRemoved <- numRemoved/length(data[[dataCol]])
  }

  datKept <- subset(datKept, select = -c(lowQ, highQ) )

  outList <- list(datKept = datKept, datRemoved = datRemoved, numRemoved = numRemoved, pRemoved = pRemoved)

  return(outList)
}
