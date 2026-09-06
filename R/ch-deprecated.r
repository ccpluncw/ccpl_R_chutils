#' Deprecated functions in chutils
#'
#' These functions still work but have been renamed. They are kept so that
#' existing analysis scripts keep running, and will be removed in a future
#' release. Each one calls its replacement and issues a deprecation warning.
#'
#' \describe{
#'   \item{\code{ch.getChlogisticXfromProbability}}{renamed to
#'     \code{\link{ch.logisticXfromY}}. The old name said the target was a
#'     probability, but \code{\link{ch.logistic}} fits continuous outcomes with
#'     free asymptotes, so the target is a y value like any other. The guard on
#'     the target was widened to match.}
#' }
#'
#' @name chutils-deprecated
#' @keywords internal
NULL

#' @rdname chutils-deprecated
#' @param modelList A list output by \code{\link{ch.logistic}}.
#' @param value A number on the y-axis whose x value you want.
#' @param range.x A vector of two numbers, c(min, max), giving the x range to
#'   search.
#' @export
ch.getChlogisticXfromProbability <- function(modelList, value = 0.5,
                                             range.x = NULL) {
  .Deprecated("ch.logisticXfromY")
  ch.logisticXfromY(modelList, value = value, range.x = range.x)
}
