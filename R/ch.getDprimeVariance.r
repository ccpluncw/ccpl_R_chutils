#' A function to get the variance from a d prime
#'
#' This function computes the variance of a d prime using Miller (1996).
#' Miller, J. (1996). The sampling distribution of d’. Perception & Psychophysics, 58, 65-72.
#' much of the code taken from Suero, M., Privado, J., & Botella, J. (2017). Methods to estimate the variance of some indices of the signal detection theory: A simulation study. Psicológica, 38(1).
#' @param NtargetPresent the number of target present trials.
#' @param NtargetAbsent the number of target absent trials.
#' @param probFA the proportion of False Alarms.
#' @param probHit the proportions of Hits.
#' @keywords dprime variance
#' @return the variance of the dPrime. Take the square root to get the SD.
#' @export
#' @examples ch.getDprimeVariance (100,100, .2, .7)

ch.getDprimeVariance <- function (NtargetPresent, NtargetAbsent, probFA, probHit) {
  fre_fa <- c(0.5,(1:(NtargetAbsent-1)), NtargetAbsent-0.5)
  fre_ht <- c(0.5,(1:(NtargetPresent-1)), NtargetPresent-0.5)

  prop_fa <- fre_fa/NtargetAbsent
  prop_ht <- fre_ht/NtargetPresent

  z_fa <- qnorm(prop_fa, mean = 0, sd = 1)
  z_ht <- qnorm(prop_ht, mean = 0, sd = 1)

  prob_fa <- dbinom(0:NtargetAbsent,NtargetAbsent,probFA)
  prob_ht <- dbinom(0:NtargetPresent,NtargetPresent,probHit)

  v_esp_zfa <- sum(z_fa*prob_fa)
  v_esp_zht <- sum(z_ht*prob_ht)
  v_esp_miller <- v_esp_zht - v_esp_zfa

  var_zfa <- sum(((z_fa*z_fa)*prob_fa))-(v_esp_zfa*v_esp_zfa)
  var_zht <- sum(((z_ht*z_ht)*prob_ht))-(v_esp_zht*v_esp_zht)

  var_miller <- var_zht + var_zfa

  return(var_miller)
}
