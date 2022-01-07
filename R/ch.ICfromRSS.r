#' A function calculate the AIC and BIC from a residual sum of squares
#'
#' This function calculates the AIC and BIC from a residual sum of squares.  The residual sum of squares, the number of data points, as well as the number of free parameters are input. ICtype specifies whether to output the AIC or BIC.
#'
#' @param rss The residual sum of squares from a model fit.
#' @param n The number of observation from which the rss was calculated.
#' @param numParameters The number of free parameters.
#' @param ICtype A string specifying whether to return the AIC or BIC.  Valid inputs are: "AIC" and "BIC".  DEFAULT = "BIC"
#''
#' @keywords BIC AIC IC Information Criterion
#' @return the AIC or BIC.
#' @export
#' @examples ch.ICfromRSS (myRss, 40, 5, ICtype = "BIC")


ch.ICfromRSS <- function(rss, n, numParameters, ICtype = "BIC") {

	#make sure minimizeStat is valid
	validOpts <- c("BIC", "AIC")
	if(!(ICtype %in% validOpts) ) {
		stop (paste("you set ICtype to:", ICtype, ", but it must be one of the following:", validOpts, sep=" "))
	}

	if (ICtype == "BIC") {
		out <- n + n * log(2 * pi) + n * log(rss/n) + log(n)*(numParameters+1)
	} else {
		out <- n * log(rss/n) + 2*numParameters
	}

	return (out)
}
