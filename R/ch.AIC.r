#' A function calculate the AIC from a model fit
#'
#' This function calculates the AIC from a model fit.  The empirical Y and the fit Y, as well as the number of free parameters are input.  The AIC is output.  If one wants standardize Y and the fit Y before calculation, then set "standardize" to TRUE.
#'
#' @param y A numeric vector containing the empirical Y from the data.
#' @param fitY A numeric vector containing the fit Y from the model.
#' @param numParameters The number of free parameters.
#' @param standardize A boolean that specifies whether to standardize the scores before calculating the AIC. Standardization is useful if you want to combine different DVs into a single AIC (e.g., RT and accuracy). DEFAULT = FALSE.
#''
#' @keywords AIC
#' @return the AIC.
#' @export
#' @examples ch.AIC (myY, fitY, 5)


# ch.AIC <- function (y, fitY, numParameters, standardize = FALSE) {
#
#   if (standardize) {
#     resids <- scale(y) - scale(fitY)
#   } else {
#     resids <- y - fitY
#   }
#   p <- numParameters
#
#   n <- length(resids)
#   rss <- sum(resids^2, na.rm = T)
#
#   AIC <- n * log(rss/n) + 2*p
#
#   return (AIC)
# }

ch.AIC <- function(y, fitY, numParameters, standardize = FALSE) {

	#get length without NAs
	n <- length(y[!is.na(y)])
	rss <- ch.RSS(y, fitY, standardize = standardize)
	AIC <- ch.ICfromRSS(rss = rss, n = n, numParameters, ICtype = "AIC")

	return (AIC)
}
