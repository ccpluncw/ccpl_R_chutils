#' Fits the p(HVO) function for the choice experiment
#'
#' This function fits p(HVO) as a non-linear exponential decay function of an x variable (often overlap). It forces the first point to equal 1 and the last point is .5 on the y-axis. It outputs the fit of the function and the r_square.
#' @param x the x variable for the x-axis (often overlap).
#' @param y the y variable for the y-axis (often p(hit)).
#' @param useTwoParameterModel A boolean that specifies whether to use a two parameter model.  If this is set to TRUE, then this function will fit a model whereby the rightmost point (overlap = 1.0) is not fixed at p(HVO) = 0.5. DEFAULT = FALSE.
#' @return a list of the fit, r2 from the nls .
#' @keywords fit p(hit) p(HVO)
#' @export
#' @examples ch.pHVOfit (x,y)

ch.pHVOfit <- function (x, y, useTwoParameterModel = FALSE) {

  nlsFit = NULL
  nlsFit.r2 = NULL

  if(useTwoParameterModel) {
    fml <- as.formula(y~(1-a)*(1-(x^b))+a)
    startList <- list(a=0.5, b=1)
  } else {
    fml <- as.formula(y~0.5*(1-(x^b))+0.5)
    startList <- list(b=1)
  }

  #make sure there are at least 3 categories on the x-axis
  if(length(x) > 2) {
    tryCatch ({
      nlsFit <- nls(fml,  start=startList, control = nls.control(minFactor=1/10000000, maxiter=10000, warnOnly = FALSE), algorithm = "port", upper = list(b=30))
    	}, error = function(e) {
    		print(paste("nls function did not fit", e))
    })
  }
  if (!is.null(nlsFit)) {
  	nlsFit.r2 <- ch.R2( y, fitY= fitted(nlsFit))
    nlsFit.r2 <- ifelse(nlsFit.r2 < 0, 0, nlsFit.r2)
    nls.beta <- coef(nlsFit)["b"]
    nls.alpha <- ifelse (is.na(coef(nlsFit)["a"]), 0.5, coef(nlsFit)["a"])
  } else {
    nlsFit.r2 <- 0
    nls.beta <- NA
    nls.alpha <- NA
  }

  return (list(nlsObject = nlsFit, beta = nls.beta, alpha = nls.alpha, r2 = nlsFit.r2))
}
