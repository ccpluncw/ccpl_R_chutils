#' Reduced Chi Square across multiple fit components (groups)
#'
#' Generalizes \code{\link{ch.reducedChiSq}} to multiple parallel fit
#' components (e.g. pHit plus several RT quantiles), each supplied as one
#' element of a list. Per-component chi-square contributions are combined
#' (optionally weighted) and divided by the pooled degrees of freedom.
#'
#' The per-component observation count is the number of terms that actually
#' enter the chi-square sum: \code{sum(!is.na((y - yFit)^2 / yVar))}. This
#' excludes cells dropped because of an NA in y, yFit, OR yVar (e.g. a tail
#' quantile whose variance could not be estimated), so the degrees of freedom
#' match the terms summed. In the single-component case with no NA variances
#' this equals \code{length(y[!is.na(y)])} and reproduces \code{ch.reducedChiSq}.
#'
#' @param yList Named list of numeric vectors: empirical Y per component.
#' @param yFitList Named list of numeric vectors: fitted Y per component (parallel to yList).
#' @param yVarList Named list of numeric vectors: variance of each Y per component (parallel to yList).
#' @param numParameters The number of free parameters.
#' @param groupWeights Optional named numeric vector of per-component weights.
#'   NULL (default) sets all weights to 1 (equal weight per observation), which
#'   reproduces \code{ch.reducedChiSq} for a single component.
#'
#' @keywords Chi Square goodness of fit
#' @return A list with the same top-level shape as \code{ch.reducedChiSq}
#'   (n, df, chiSq, pValueChiSq, reducedChiSq) plus \code{perGroup}: a data frame
#'   of per-component group name, n, chiSq, and weight.
#' @export
#' @examples
#' ch.reducedChiSqMulti(
#'   list(pHit = c(0.8, 0.6), RT_Q50 = c(1.2, 0.9)),
#'   list(pHit = c(0.7, 0.65), RT_Q50 = c(1.1, 1.0)),
#'   list(pHit = c(0.01, 0.01), RT_Q50 = c(0.02, 0.02)),
#'   numParameters = 3)

ch.reducedChiSqMulti <- function(yList, yFitList, yVarList, numParameters,
                                 groupWeights = NULL) {
  gnames <- names(yList)
  if (is.null(gnames)) gnames <- as.character(seq_along(yList))
  nG <- length(yList)
  w <- if (is.null(groupWeights)) stats::setNames(rep(1, nG), gnames)
       else groupWeights[gnames]

  chi_g <- numeric(nG); n_g <- numeric(nG)
  for (g in seq_len(nG)) {
    term     <- (yList[[g]] - yFitList[[g]])^2 / yVarList[[g]]
    n_g[g]   <- sum(!is.na(term))            # terms that actually enter the sum
    chi_g[g] <- sum(term, na.rm = TRUE)
  }

  chi.total <- sum(w * chi_g)
  n.total   <- sum(w * n_g)
  df        <- n.total - numParameters
  chi.reduced <- chi.total / df
  pChiSq <- stats::pchisq(chi.total, df, lower.tail = FALSE)

  list(n = n.total, df = df, chiSq = chi.total, pValueChiSq = pChiSq,
       reducedChiSq = chi.reduced,
       perGroup = data.frame(group = gnames, n = n_g, chiSq = chi_g,
                             weight = as.numeric(w), row.names = NULL))
}
