#' A function calculates the min and max values for a plot axis
#'
#' This function calculates the min and max values for a plot axis with a bit of a visual buffer.
#' @param variable a numeric array from which you the axis values are derived (usually just a vector).
#' @param p(buffer) the proportion of the axis range that you want to leave buffered on the low and the high end
#' @keywords plot axis buffer
#' @return a vector containing the min and max values for the axis.
#' @export
#' @examples ch.getPlotAxisMinMax (xAxisVals)

ch.getPlotAxisMinMax <- function (x, pBuffer = 0.1) {

  buffer = (max(x, na.rm=T) - min(x, na.rm=T)) * pBuffer
  minV = min(x, na.rm=T) - buffer
  maxV = max(x, na.rm=T) + buffer

  return(c(minV, maxV))
}
