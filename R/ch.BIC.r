#' A function calculate the BIC from a model fit
#'
#' This function calculates the BIC from a model fit.  The empirical Y and the fit Y, as well as the number of free parameters are input.  The BIC is output.  BIC is a function of the number of observations. If one wants correct for the number of observations, then set "standardize" to TRUE.
#'
#' @param y A numeric vector containing the empirical Y from the data.
#' @param fitY A numeric vector containing the fit Y from the model.
#' @param numParameters The number of free parameters.
#' @param standardize BIC is a function of the number of observations. If you wants correct for the number of observations, then set "standardize" to TRUE. DEFAULT = FALSE.
#''
#' @keywords BIC
#' @return the BIC.
#' @export
#' @examples ch.BIC (myY, fitY, 5)


ch.BIC <- function(y, fitY, numParameters, standardize = FALSE) {

	resids <- y - fitY
	p <- numParameters
	###
	# resids = a vector of residuals from your model fit
	# p = number of free parameters
	#####

	n <- length(resids)
	rss <- sum(resids^2, na.rm = T)

	BIC <- n + n * log(2 * pi) + n * log(rss/n) + log(n)*(p+1)

	if (standardize) {
		BIC <- BIC/n
	}
	return (BIC)
}
