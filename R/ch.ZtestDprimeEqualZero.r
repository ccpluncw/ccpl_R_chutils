#' A function tests whether a dPrime is significantly Greater than/different from Zero
#'
#' This function tests whether a dPrime is significantly Greater than/different from Zero. It calculates variance using:
#' Miller, J. (1996). The sampling distribution of d’. Perception & Psychophysics, 58, 65-72.
#' @param NtargetPresent the number of target present trials.
#' @param NtargetAbsent the number of target absent trials.
#' @param probFA the proportion of False Alarms.
#' @param probHit the proportions of Hits.
#' @param oneTailed a boolean specifying whether the test is one tailed.  DEFAULT = TRUE (because we assume that dPrimes are bounded by Zero on the low end).
#' @keywords dprime z test different zero
#' @return a vector containing the z value (zVal) and the p value (p val) of the test.
#' @export
#' @examples ch.ZtestDprimeEqualZero (100,100, .2, .7)

ch.ZtestDprimeEqualZero <- function (NtargetPresent, NtargetAbsent, probFA, probHit, oneTailed = TRUE) {

  dPrime <- qnorm(probHit)-qnorm(probFA)
  var <- ch.getDprimeVariance (NtargetPresent, NtargetAbsent, probFA, probHit)
  SE <- sqrt(var)
  zVal <- dPrime/SE
  if(oneTailed) {
    pVal <- pnorm(-1*zVal)
  } else {
    pVal <- 2*pnorm(-abs(zVal))
  }
  return(c(zVal = zVal, pVal = pVal))
}
