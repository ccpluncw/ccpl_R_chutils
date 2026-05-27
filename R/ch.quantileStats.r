#' Compute sample quantiles with variance estimates
#'
#' For each requested quantile, returns the sample quantile and an estimate of
#' its sampling variance, computed either by bootstrap (default) or the
#' Maritz-Jarrett method. Used by the RRW2 multi-quantile fitting pipeline to
#' build per-cell RT quantile targets and the variances that weight them.
#'
#' @param x A numeric vector of observations (e.g. per-cell RT residuals).
#' @param quantiles A numeric vector of probabilities in (0,1). Default 0.5.
#' @param n_boot Number of bootstrap resamples (bootstrap method only). Default 1000.
#' @param method Variance estimator: "bootstrap" (default) or "mj" (Maritz-Jarrett).
#' @param na.rm Logical; drop NAs before computing. Default TRUE.
#'
#' @keywords quantile bootstrap variance
#' @return A named list keyed by paste0("Q", round(q*100)). Each element is a
#'   list with `value` (the sample quantile) and `var` (its variance estimate).
#'   `var` is NA when fewer than 2 non-NA observations are available.
#' @export
#' @examples
#' ch.quantileStats(rnorm(500), quantiles = c(0.1, 0.5, 0.9))

ch.quantileStats <- function(x, quantiles = 0.5, n_boot = 1000,
                             method = c("bootstrap", "mj"), na.rm = TRUE) {
  method <- match.arg(method)
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  qnames <- paste0("Q", round(quantiles * 100))

  values <- stats::quantile(x, probs = quantiles, type = 7, names = FALSE,
                            na.rm = FALSE)

  if (n < 2) {
    vars <- rep(NA_real_, length(quantiles))
  } else if (method == "bootstrap") {
    boot_qs <- replicate(n_boot,
      stats::quantile(sample(x, n, replace = TRUE), probs = quantiles,
                      type = 7, names = FALSE, na.rm = FALSE))
    boot_qs <- matrix(boot_qs, nrow = length(quantiles))   # L x n_boot
    vars <- apply(boot_qs, 1, stats::var)
  } else {                                  # Maritz-Jarrett SE (Wilcox mjse)
    xs <- sort(x); i <- seq_len(n)
    vars <- vapply(quantiles, function(p) {
      m <- min(max(floor(p * n + 0.5), 1), n - 1)   # clamp to keep beta shapes valid
      a <- m - 1; b <- n - m
      w <- stats::pbeta(i / n, a, b) - stats::pbeta((i - 1) / n, a, b)
      c1 <- sum(w * xs); c2 <- sum(w * xs^2)
      max(c2 - c1^2, 0)                              # variance (mjse^2)
    }, numeric(1))
  }

  stats::setNames(
    lapply(seq_along(quantiles),
           function(j) list(value = values[j], var = vars[j])),
    qnames)
}
