#' A function calculate the reduced Chi Square from a model fit
#'
#' This function calculates the reduced Chi Square from a from a model fit.  The empirical Y, y variance, and the fit Y, as well as the number of free parameters are input.
#'
#' This is the single-component case of \code{\link{ch.reducedChiSqMulti}}; the
#' body wraps that function with one group and returns the same fields as
#' before (the per-group breakdown is dropped). Behavior is unchanged for the
#' historical single-RT pipeline; see \code{ch.reducedChiSqMulti} for the
#' degrees-of-freedom convention when variances can be NA.
#'
#' @param y A numeric vector containing the empirical Y from the data.
#' @param yFit A numeric vector containing the fit Y from the model.
#' @param yVar A numeric vector containing the variance of each row in the Y vector.
#' @param numParameters The number of free parameters.
#''
#' @keywords Chi Square goodness of fit
#' @return a list containing the number of observations (n); the degrees of freedom (df), the chi square statistic (chiSq), the p value associated with the chi square and df, and the reduced chi square.
#' @export
#' @examples ch.reducedChiSq (myY, fitY, yVaraince, 5)

ch.reducedChiSq <- function(y, yFit, yVar, numParameters) {
	res <- ch.reducedChiSqMulti(list(y), list(yFit), list(yVar), numParameters)
	res$perGroup <- NULL
	return(res)
}