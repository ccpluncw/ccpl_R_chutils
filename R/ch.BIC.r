#' A function calculate the BIC from a model fit
#'
#' This function calculates the BIC from a model fit.  The empirical Y and the fit Y, as well as the number of free parameters are input.  The BIC is output.  If one wants standardize Y and the fit Y before calculation, then set "standardize" to TRUE.
#'
#' @param y A numeric vector containing the empirical Y from the data.
#' @param fitY A numeric vector containing the fit Y from the model.
#' @param numParameters The number of free parameters.
#' @param standardize A boolean that specifies whether to standardize the scores before calculating the BIC. Standardization is useful if you want to combine different DVs into a single BIC (e.g., RT and accuracy). DEFAULT = FALSE.
#''
#' @keywords BIC
#' @return the BIC.
#' @export
#' @examples ch.BIC (myY, fitY, 5)

ch.BIC <- function(y, fitY, numParameters, standardize = FALSE) {

	#get length without NAs
	n <- length(y[!is.na(y)])
	rss <- ch.RSS(y, fitY, standardize = standardize)
	BIC <- ch.ICfromRSS(rss = rss, n = n, numParameters, ICtype = "BIC")

	return (BIC)
}
