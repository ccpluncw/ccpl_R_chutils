#' Fits the p(HVO) function for the choice experiment
#'
#' This function fits p(HVO) as a non-linear exponential decay function of an x variable (often overlap). It forces the first point to equal 1 and the last point is .5 on the y-axis. It outputs the fit of the function and the r_square.
#' @param x the x variable for the x-axis (often overlap).
#' @param y the y variable for the y-axis (often p(hit)).
#' @param grp the grouping variable that identifies those stimuli whose value are above the reference distribution and those that are below the reference distribution. When this variable is included, then the intercept parameter is allowed to vary for refHVO and refLVO, but the slope is kept constant. DEFAULT = NULL (the grouping variable is ignored)
#' @return a list of the fit, r2 from the nls .
#' @keywords fit p(hit) p(HVO)
#' @export
#' @examples ch.RTfit (x,y, grp)

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
