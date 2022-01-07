#' A function calculate the AIC and BIC from a model fit
#'
#' This function calculates the AIC and BIC from a from a model fit.  The empirical Y and the fit Y, as well as the number of free parameters are input.   If one wants standardize Y and the fit Y before calculation, then set "standardize" to TRUE. ICtype specifies whether to output the AIC or BIC.
#'
#' @param y A numeric vector containing the empirical Y from the data.
#' @param fitY A numeric vector containing the fit Y from the model.
#' @param numParameters The number of free parameters.
#' @param standardize A boolean that specifies whether to standardize the scores before calculating the BIC. Standardization is useful if you want to combine different DVs into a single BIC (e.g., RT and accuracy). DEFAULT = FALSE.
#' @param ICtype A string specifying whether to return the AIC or BIC.  Valid inputs are: "AIC" and "BIC".  DEFAULT = "BIC"
#''
#' @keywords BIC AIC IC Information Criterion
#' @return the AIC or BIC.
#' @export
#' @examples ch.IC (myY, fitY, 5, ICtype = "AIC")

ch.IC <- function(y, fitY, numParameters, standardize = FALSE, ICtype = "BIC") {

	#make sure minimizeStat is valid
	validOpts <- c("BIC", "AIC")
	if(!(ICtype %in% validOpts) ) {
		stop (paste("you set ICtype to:", ICtype, ", but it must be one of the following:", validOpts, sep=" "))
	}

	#get length without NAs
	n <- length(y[!is.na(y)])
	rss <- ch.RSS(y, fitY, standardize = standardize)
	ICout <- ch.ICfromRSS(rss = rss, n = n, numParameters=numParameters, ICtype = ICtype)

	return (ICout)
}
