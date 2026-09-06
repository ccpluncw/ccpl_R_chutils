#' Fits a linear model of a response variable on an x variable
#'
#' This function fits y as a linear function of x using \code{\link{lm}}, and
#' returns the fitted object alongside its slope, intercept and r_square. It was
#' written for response times, but nothing in it is specific to RT. When a
#' two-level grouping variable is supplied the group is coded 1/-1 and entered
#' as an additive term, so the two groups are fit as parallel lines: a common
#' slope with the intercept shifted by +delta and -delta.
#' @param x the x variable for the x-axis (often overlap).
#' @param y the y variable for the y-axis (often RT).
#' @param grp a grouping variable with exactly two levels. When it is included,
#'   the intercept is allowed to differ between the two groups but the slope is
#'   kept constant. A grouping variable with any other number of levels is
#'   ignored with a warning. DEFAULT = NULL (the grouping variable is ignored)
#' @return a list with RTObject (the lm fit, or NULL if it did not fit), beta
#'   (the slope on x), alpha (the intercept), delta (half the difference between
#'   the group intercepts, 0 when no grouping variable was used) and r2. When
#'   the fit fails, r2 is 0 and the coefficients are NA.
#' @keywords fit linear model RT
#' @seealso \code{\link{ch.R2}}, \code{\link{ch.plotTwoLinearFits}}
#' @export
#' @examples
#' x <- rep(1:10, 5)
#' y <- 2 * x + rnorm(length(x))
#' ch.RTfit(x, y)

ch.RTfit <- function (x, y, grp = NULL) {

  RTFit = NULL
  RTFit.r2 = NULL

  if(is.null(grp)) {
    fml <- as.formula(y~x)
  } else {
    grp.names <- unique(grp)
    if(length(grp.names) != 2) {
      warning(paste("Grouping variable must have only 2 levels. Your grouping variable has ", length(grp.names), " levels. Skipping Grouping", sep=""))
      fml <- as.formula(y~x)
    } else {
      grpCode <- ifelse(grp == grp.names[1], 1, -1)
        fml <- as.formula(y~grpCode + x)
    }
  }

  #make sure there are at least 3 categories on the x-axis
  if(length(x) > 2) {
    tryCatch ({
      RTFit <- lm(fml)
    	}, error = function(e) {
    		print(paste("lm function did not fit", e))
    })
  }
  if (!is.null(RTFit)) {
  	RTFit.r2 <- ch.R2( y, fitY= fitted(RTFit))
    RTFit.r2 <- ifelse(RTFit.r2 < 0, 0, RTFit.r2)
    RT.beta <- coef(RTFit)["x"]
    RT.alpha <- coef(RTFit)["(Intercept)"]
    RT.delta <- ifelse (is.na(coef(RTFit)["grpCode"]), 0, coef(RTFit)["grpCode"])
  } else {
    RTFit.r2 <- 0
    RT.beta <- NA
    RT.alpha <- NA
    RT.delta <- NA
  }

  return (list(RTObject = RTFit, beta = RT.beta, alpha = RT.alpha, delta = RT.delta, r2 = RTFit.r2))
}
