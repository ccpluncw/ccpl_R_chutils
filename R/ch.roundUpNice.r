#' A function to round pretty
#'
#' This function rounds up numbers pretty.
#' @param x the number to be rounded.
#' @param Nice a vector specifying the rounding parameters. The default values should work.
#' @keywords Round up pretty
#' @return the rounded number.
#' @export
#' @examples cd.roundUpNice (6)


ch.roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}
