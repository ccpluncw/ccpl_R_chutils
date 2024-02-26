#' A function calculate the residual sum of squares from a model fit
#'
#' This function calculates the residual sum of squares from a model fit.  The empirical Y and the fit Y are input.  The RSS is output.   If one wants to standardize the variables before calculation, then set "standardize" to TRUE.
#'
#' @param y A numeric vector containing the empirical Y from the data.
#' @param fitY A numeric vector containing the fit Y from the model.
#' @param standardize A boolean that specifies whether to standardize the scores before calculating the RSS. Standardization is useful if you want to combine different DVs into a single RSS (e.g., RT and accuracy). DEFAULT = FALSE.
#''
#' @keywords residual sum of squares RSS
#' @return the RSS.
#' @export
#' @examples ch.RSS (myY, fitY)


ch.RSS <- function(y, fitY, standardize = FALSE) {

		if (standardize) {
			df.z <- standardizeDataAndFit(y, fitY)
			y <- df.z$data
			fitY <- df.z$fit
		}

		#make sure there are predicted values
		if(length(na.omit(fitY)) > 1) {
			rss <- sum( (y - fitY)^2, na.rm = T)
		} else {
			#if not, then set rss equal to tss so r2 = 0
			rss <- NA
		}

	return (rss)
}
