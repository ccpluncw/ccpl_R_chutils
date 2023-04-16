#' A function bin a vector of numbers
#'
#' This function creates bins for a vector of numbers.
#'
#' @param data A numeric vector.
#' @param numBins The number of bins you want the vector divided into.
#' @param centerLabels A boolean, if TRUE will create labels that are the center of the bin. If FALSE, the labels will be the max number in the bin. DEFAULT = TRUE
#''
#' @keywords bin vector
#' @return A vector of bins that match to the rows of "data".
#' @export
#' @examples ch.binNumbers (rnorm(100, 10, 2), 10)


ch.binNumbers <- function(data, numBins, centerLabels = TRUE) {

  interval <- (max(data) - min(data))/numBins
  breaks <- seq(min(data), max(data), interval)
  if(centerLabels) {
    labels <- round(breaks[2:length(breaks)] - (interval/2),2)
  } else {
    labels <- round(breaks[2:length(breaks)],2)
  }

  breaks[1] <- -Inf
  breaks[length(breaks)] <- Inf

  outVec <- cut(data, breaks = breaks, labels = labels)
  return(as.numeric(as.character(outVec)))
}
