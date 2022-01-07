#' A function calculate the total sum of squares from a vector
#'
#' This function calculates the total sum of squares from a vector.  The empirical Y is input.  The TSS is output.   If one wants to standardize the variable before calculation, then set "standardize" to TRUE.
#'
#' @param y A numeric vector containing the empirical Y from the data.
#' @param standardize A boolean that specifies whether to standardize the scores before calculating the RSS. Standardization is useful if you want to combine different DVs into a single RSS (e.g., RT and accuracy). DEFAULT = FALSE.
#''
#' @keywords total sum of squares TSS
#' @return the TSS.
#' @export
#' @examples ch.TSS (myY)

ch.TSS <- function(y, standardize = FALSE) {

	if (standardize) {
		y <- scale(y)
	}
	meanY <- mean(y, na.rm=T)
	tss <- sum( (y - meanY)^2, na.rm = T)

	return (tss)
}
