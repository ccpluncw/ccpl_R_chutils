#' A function calculate the variance of a proportion
#'
#' This function calculates the variance of a proportion.    
#'
#' @param p A numeric vector containing the proportions that variance will be calculated.
#' @param n A numeric vector containing the number of observations for each proportion.
#''
#' @keywords proportion variance
#' @return a numeric vector of the variances for each proportion in p.
#' @export
#' @examples ch.propVar (c(0.5, 0.2, 0.1), c(100, 150, 50))

ch.propVar <- function(p, n) {
	out <- (p*(1-p))/n
	return(out)
}