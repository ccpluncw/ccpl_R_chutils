#' A function bins a vector of numbers
#'
#' This function creates N bins for a vector of numbers. It equalized the number of elements in each bin, so the intervals the each bin contains may vary based on the distribution of data. The bins are labeled by the average value of the items in the bin.
#'
#' @param data A numeric vector.
#' @param numBins The number of bins you want the vector divided into.
#''
#' @keywords bin vector
#' @return A vector of bins that match to the rows of "data".
#' @export
#' @examples ch.binByNumElements (rnorm(100, 10, 2), 10)

ch.binByNumElements <- function(data, numBins) {

  uniqueElements <- unique(data)
  numElementsInBin <- ceiling(length(uniqueElements)/numBins)
  uniqueElements <- sort(uniqueElements)

  breaks <- rep(NA, numBins+1)
  labels <- rep(NA, numBins)
  for(i in 1:numBins) {
    max.1 <- ifelse((numElementsInBin*i) <= length(uniqueElements), (numElementsInBin*i), length(uniqueElements))
    min.1 <- ((numElementsInBin*(i-1))+1)
    breaks[i] <- ifelse(i == 1, -Inf, mean(uniqueElements[min.1 - 1], uniqueElements[min.1]))
    labels[i]<- mean(uniqueElements[min.1:max.1], na.rm = T)
  }

  breaks[length(breaks)] <- Inf
  outVec <- cut(data, breaks = breaks, labels = labels)
  return(as.numeric(as.character(outVec)))
}
