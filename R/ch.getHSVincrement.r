#' A function to calculate the increment for HSV color based numChages needed
#'
#' This function calculates the increment for HSV color based numChages needed.
#' @param numChanges an integer denoting the number of changes required.
#' @param bounds the min and max of the h or s or v (usually 0 and 1).
#' @keywords hsv increment changes groups
#' @return decimal indicating the hsv increment
#' @export
#' @examples ch.getHSVincrement(3, c(1,0))


ch.getHSVincrement <- function (numChanges, bounds = c(min, max)) {

   if((numChanges -  1) > 0) {
     out <- (bounds[2]-bounds[1])/(numChanges -  1)
   } else {
     out <- 0
   }

   return (out)

}
