#' A function calculate the r square from a model fit
#'
#' This function calculates the r2 from a from a model fit.  The empirical Y and the fit Y are input.   If one wants standardize Y and the fit Y before calculation, then set "standardize" to TRUE.
#'
#' @param y A numeric vector containing the empirical Y from the data.
#' @param fitY A numeric vector containing the fit Y from the model.
#' @param standardize A boolean that specifies whether to standardize the scores before calculating the BIC. Standardization is useful if you want to combine different DVs into a single BIC (e.g., RT and accuracy). DEFAULT = FALSE.
#''
#' @keywords r2 r_square r square
#' @return the r square.
#' @export
#' @examples ch.r2 (myY, fitY)

ch.R2 <- function(y, fitY, standardize = FALSE) {

	#get length without NAs
	rss <- ch.RSS(y, fitY, standardize = standardize)
	tss <- ch.TSS(y, standardize = standardize)

	#return NA if rss is NA
	r2 <- ifelse(is.na(rss), NA, 1-(rss/tss))

	return (r2)
}
