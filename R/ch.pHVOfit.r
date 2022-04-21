#' Fits the p(HVO) function for the choice experiment
#'
#' This function fits p(HVO) as a non-linear exponential decay function of an x variable (often overlap). It forces the first point to equal 1 and the last point is .5 on the y-axis. It outputs the fit of the function and the r_square.
#' @param x the x variable for the x-axis (often overlap).
#' @param y the y variable for the y-axis (often p(hit)).
#' @param grp the grouping variable that identifies those stimuli whose value are above the reference distribution and those that are below the reference distribution. When this variable is included and useTwoParameterModel is set to TRUE, then the alpha parameter will be fit so it is symetric above and below 0.5 for refHVO and refLVO. DEFAULT = NULL (the grouping variable is ignored)
#' @param useTwoParameterModel A boolean that specifies whether to use a two parameter model.  If this is set to TRUE, then this function will fit a model whereby the rightmost point (overlap = 1.0) is not fixed at p(HVO) = 0.5. DEFAULT = FALSE.
#' @return a list of the fit, r2 from the nls .
#' @keywords fit p(hit) p(HVO)
#' @export
#' @examples ch.pHVOfit (x,y, grp)

ch.pHVOfit <- function (x, y, grp = NULL, useTwoParameterModel = FALSE) {

  nlsFit = NULL
  nlsFit.r2 = NULL

  #if use two parameter model
  if(useTwoParameterModel) {
    #and there is no grouping variable
    if(is.null(grp)) {
      #run a single two parameter model
      fml <- as.formula(y~(1-a)*(1-(x^b))+a)
      startList <- list(a=0.5, b=1)
    } else {
      #if there is a grouping variable, then check that there are exactly 2 groups
      grp.names <- unique(grp)
      #if there are not exactly two groups
      if(length(grp.names) != 2) {
        warning(paste("Grouping variable must have only 2 levels. Your grouping variable has ", length(grp.names), " levels. The levels are: ", paste(grp.names, collapse=", "), " Skipping Grouping", sep=""))
        #then run a single two parameter model (ignore the grouping variable)
        fml <- as.formula(y~(1-a)*(1-(x^b))+a)
        startList <- list(a=0.5, b=1)
      } else {
        #if there are exactly two groups, then create an effect coding variable for the grouping variable
        grpCode <- ifelse(grp == grp.names[1], 1, -1)
        #and fit a model whereby there is a symmetry between each group around the chance line
        fml <- as.formula(y~(1-(0.5 + (aDelta*grpCode)))*(1-(x^b))+(0.5 + (aDelta*grpCode)))
        startList <- list(b=1, aDelta=0)
      }
    }
  } else {
    #if you are fitting only one parameter model, check if there is a grouping variable
    if(is.null(grp)) {
      #if not, then fit a single function
      fml <- as.formula(y~0.5*(1-(x^b))+0.5)
      startList <- list(b=1)
    } else {
      #if there is a grouping variable, then check that there are exactly 2 groups
      grp.names <- unique(grp)
      #if there are not exactly two groups
      if(length(grp.names) != 2) {
        warning(paste("Grouping variable must have only 2 levels. Your grouping variable has ", length(grp.names), " levels. The levels are: ", paste(grp.names, collapse=", "), " Skipping Grouping", sep=""))
        #then run a single one parameter model (ignore the grouping variable)
        fml <- as.formula(y~0.5*(1-(x^b))+0.5)
        startList <- list(b=1)
      } else {
        #if there are exactly two groups, then create an effect coding variable for the grouping variable
        grpCode <- ifelse(grp == grp.names[1], 1, -1)
        #and fit a model whereby each group is fit with a single one parameter model
        fml <- as.formula(y~0.5 * (1-(x^(b + (grpCode*bDelta))))+0.5)
        startList <- list(b=1, bDelta=0)
      }
    }
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
    nls.aDelta <- ifelse (is.na(coef(nlsFit)["aDelta"]), 0, coef(nlsFit)["aDelta"])
    nls.bDelta <- ifelse (is.na(coef(nlsFit)["bDelta"]), 0, coef(nlsFit)["bDelta"])
  } else {
    nlsFit.r2 <- 0
    nls.beta <- NA
    nls.alpha <- NA
    nls.aDelta <- NA
    nls.bDelta <- NA
  }

  return (list(nlsObject = nlsFit, beta = nls.beta, alpha = nls.alpha, aDelta = nls.aDelta, bDelta = nls.bDelta, r2 = nlsFit.r2))
}
