#' A function to get the X value associated with a specified probability from ch.logistic
#'
#' This function gets the X value associated with a specified probability from ch.logistic
#' @param modelList A list output by ch.logistic.
#' @param pValue A number between 0 and 1 that is the probability that you are interested in. DEFAULT = 0.5
#' @param range.x A vector of two numbers, c(min, max), that specifies the range of x to look for pValue. If NULL or invalid, then the range will be the min and max of the x vector. DEFAULT = NULL
#' @keywords logistic function probability
#' @return a number that is the X value associated with the input pValue.
#' @export
#' @examples ch.getChlogisticXfromProbability (modelList, 0.3)

ch.getChlogisticXfromProbability <- function(modelList, value = 0.5, range.x = NULL) {

	findInt <- function(modelList, shift, scale, value) {
	    function(x) {
	        predict(modelList$fit, data.frame(x=x, shift = shift, scale = scale), type="response") - value
	     }
	}

	if(is.null(range.x)) range.x <- range(modelList$data$x)

	out <- NA
	if(value > 0 & value < 1) {
		shift <- modelList$data$shift[1]
		scale <- modelList$data$scale[1]

		out <- tryCatch({
									 uniroot(findInt(modelList, shift, scale, value = value), range.x)$root
								}, error = function(modelList) {
								            message("ch.getChlogisticXfromProbability failed")
								            # Choose a return value in case of error
								            NA
								})
	}

	return(out)
}
