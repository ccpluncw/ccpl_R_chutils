#' A function calculate the reduced Chi Square from a model fit
#'
#' This function calculates the reduced Chi Square from a from a model fit.  The empirical Y, y variance, and the fit Y, as well as the number of free parameters are input.   
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
	y.n <- length(y[!is.na(y)])

	df <- y.n - numParameters	
	chi <- sum( (y - yFit)^2/yVar, na.rm=T)
	chi.reduced <- chi/df
	
	pChiSq <- pchisq(chi, df, lower.tail = FALSE)
	
	outlist <- list(n = y.n, df = df, chiSq = chi, pValueChiSq = pChiSq, reducedChiSq = chi.reduced)
	
	return(outlist)
}