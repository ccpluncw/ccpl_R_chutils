#' A function to transform a variable that contains 0s and/or negatives, while retaining ordinality, 0==0,
#' and symmetry of influence around 0
#'
#' This function transforms a variable as follows: x.out <- ifelse(x>0, x^(1/root), -1*abs(x)^(1/root)). This transformation compresses numbers that are a large distance from zero (both positive and negative).
#' @param x a numeric vector.
#' @param root a number indicating the root that x will be taken to (e.g., 2 = sqrt; 4=cubed root; etc.). DEFAULT = 4.
#' @keywords alternative sqrt transform zero negative
#' @return a vector of transformed numbers.
#' @export
#' @examples ch.altRootTransform (myX)


ch.altRootTransform <- function (x, root = 4) {

  x.out <- ifelse(x>0, x^(1/root), -1*abs(x)^(1/root))

  return(x.out)

}
