#' A function calculate the normalized residuals (resid/sd) from a model fit
#'
#' This function calculates the normalized residuals from a from a model fit.  The empirical Y, y variance, and the fit Y are input.
#'
#' @param y A numeric vector containing the empirical Y from the data.
#' @param yFit A numeric vector containing the fit Y from the model.
#' @param yVar A numeric vector containing the variance of each row in the Y vector.
#''
#' @keywords residuals normalized
#' @return a vectpr containing the normalized residuals.
#' @export
#' @examples ch.getNormalizedResiduals (myY, fitY, yVaraince)

ch.getNormalizedResiduals <- function(y, yFit, yVar) {
	yVar <- ifelse(yVar == 0, NA, yVar)
	out <- (y - yFit)/sqrt(yVar)
	return(out)
}
