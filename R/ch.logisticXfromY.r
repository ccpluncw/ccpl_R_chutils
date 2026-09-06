#' Get the X value at which a ch.logistic fit reaches a specified Y
#'
#' Inverse prediction for a fit from \code{\link{ch.logistic}}: given a value on
#' the y-axis, find the x at which the fitted curve crosses it, by root-finding
#' on the fitted function.
#'
#' Because \code{\link{ch.logistic}} can be fit with free \code{bottom} and
#' \code{top} parameters, the fitted curve does not necessarily run between 0
#' and 1, and \code{value} is checked against the range the fitted curve
#' actually covers rather than against (0, 1). A \code{value} outside that range
#' has no crossing, so the function returns NA with a message.
#'
#' @param modelList A list output by \code{\link{ch.logistic}}.
#' @param value A number on the y-axis, inside the range of the fitted curve,
#'   whose x value you want. DEFAULT = 0.5
#' @param range.x A vector of two numbers, c(min, max), that specifies the range
#'   of x to search. If NULL, the range will be the min and max of the x vector.
#'   DEFAULT = NULL
#' @keywords logistic function inverse prediction
#' @return a number that is the X value at which the fit reaches \code{value},
#'   or NA if there is no crossing in \code{range.x}.
#' @seealso \code{\link{ch.logistic}}
#' @export
#' @examples
#' x <- rep(seq(0, 1, 0.1), 20)
#' y <- 1/(1 + exp(5 - 10*x)) + rnorm(length(x), 0, 0.02)
#' fit <- ch.logistic(x, y)
#' ch.logisticXfromY(fit, 0.3)

ch.logisticXfromY <- function(modelList, value = 0.5, range.x = NULL) {

	#shift and scale are only data columns when they were fixed, so build
	#newdata from whichever of them this fit actually has
	findInt <- function(modelList, value) {
	    function(x) {
	        nd <- data.frame(x = x)
	        for (cl in c("shift", "scale")) {
	            if (!is.null(modelList$data[[cl]])) nd[[cl]] <- modelList$data[[cl]][1]
	        }
	        predict(modelList$fit, nd) - value
	     }
	}

	if(is.null(range.x)) range.x <- range(modelList$data$x)

	#the target has to be inside the range the curve covers. with free
	#bottom and top that is not always (0,1)
	fit.range <- range(modelList$data$Fit, na.rm = TRUE)

	out <- NA
	if(is.finite(value) && value > fit.range[1] && value < fit.range[2]) {
		out <- tryCatch({
									 uniroot(findInt(modelList, value = value), range.x)$root
								}, error = function(modelList) {
								            message("ch.logisticXfromY failed")
								            # Choose a return value in case of error
								            NA
								})
	} else {
		#say why, so a bad target does not look like a failed fit
		message("ch.logisticXfromY: y = ", value, " is outside the fitted range (",
		        signif(fit.range[1], 4), ", ", signif(fit.range[2], 4), ")")
	}

	return(out)
}
