#' A function to log transform a variable that contains 0s and/or negatives
#'
#' This function log transform a variable that contains 0s and/or negatives by first tranforming x == 0 into min(x[x>0])/2 and then transforming negative numbers into (min(x[x>0])/2)/abs(x).  Finally those numbers are log transformed.
#' @param x a numeric combnVector.
#' @keywords alternative log transform zero negative
#' @return a vector of transformed numbers.
#' @export
#' @examples ch.altLogTransform (myX)


ch.altLogTransform <- function (x) {
  minvar <- min(x[x>0])
  MinResp <- minvar/2
  x <- ifelse(x==0, MinResp, x)
  x <- ifelse(x<0, MinResp/abs(x), x)
  x.out <- log(x)

  return(x.out)

}
