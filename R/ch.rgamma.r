#' A function to generate random numbers from a gamma distribution using the mean and sd as the input (rather than shape and scale)
#'
#' This function to generate random numbers from a gamma distribution using the mean and sd as the input (rather than shape and scale)
#'
#' @param n The number of observations.
#' @param m The mean of the gamma distribution.
#' @param s The standard deviation of the gamma distribution.
#''
#' @keywords rgamma random gamma distribution
#' @return A vector of observations from the requested gamma distribution.
#' @export
#' @examples ch.rgamma (1000, 2, 0.5)

ch.rgamma <- function(n, mean, sd) {
  shape <- (mean/sd)^2
  scale <- sd^2/mean
  out <- rgamma(n, shape = shape, scale = scale)
  return (out)
}
