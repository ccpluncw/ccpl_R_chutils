#' A function to fit a logistic function
#'
#' This function fits a logistic function to a dataset
#' @param x A vector of predictor values.
#' @param y A vector outcome values.
#' @param parameters A vector that specifies the free parameters that are used in the logistic model. There are four possible free parameters, three of them are specified here: bottom, top, and slope (The fourth is a scale value and is set using fixedMaxX). If you specify the variable value in this vector, then it will be included in the logistic model as a free parameter. Furthermore, the value you set in this vector is the start value for the maximum likelihood search. If you do not specify it, or assign in an NA, then it be fixed at the default value.  bottom: specifies the floor value. DEFAULT = 0. top: specifies the ceiling value. DEFAULT = 1. slope: is always a free value.  You have the opportunity to specify its starting value here.
#' @param fixedMaxX A numeric value that specifies a fixed X value for the function to reach ceiling.  IF this parameter is set to NULL, then scale will be a free parameter and maximum x value for ceiling will be derived from the dataset. DEFAULT = NULL.
#' @param fixedMinX A numeric value that specifies the fixed x value for the function to reach floor.  This value must be set and is used to transform the x variable so that the logistic will run properly.  If you are unsure, then set this value to min(x).  DEFAULT = 0.
#' @keywords logistic function
#' @return a list that contains the input data (data) with the best fit values (column name = "fit") and a scale value if scale is fixed (column name = "scale"); the nls object (fit); and a measure of the variance explained by the fitted function (r2).
#' @export
#' @examples ch.logistic (x, y, c(bottom = 0, slope = 5), fixedMinX = -1)


ch.logistic <- function(x, y, parameters = c(bottom = NA, top = NA, slope = 5), fixedMaxX = NULL, fixedMinX = 0) {
  data <- data.frame(x = x, y = y)
  df.tmp <- data

  start <- NULL
  if(is.null(fixedMaxX)) {
    #derive maxX from the data with a free parameter
    #to fill the scale start point, one must find the max value of the x parameter.
    start["scale"] <- 10/max(df.tmp$x)
  } else {
    #correct maxX for the influence of minX
    maxX <- fixedMaxX - fixedMinX
    df.tmp$scale = 10/maxX
  }

  if(fixedMinX != 0) {
    #transform x so it starts at 0
    df.tmp$x <- df.tmp$x - fixedMinX
  }


  ### if both a and d parameters have a start value do the following
  if(!is.na(parameters["bottom"]) & !is.na(parameters["top"])) {
    start["bottom"] <- parameters["bottom"]
    start["top"] <- parameters["top"]
    fml <- as.formula(y ~ bottom + (top-bottom)/(1+exp(slope-scale*x)))
  } else {
    #check if a or d exists, but not both
    if(!is.na(parameters["bottom"]) | !is.na(parameters["top"])) {
      #check if an a parameter exists
      if(!is.na(parameters["bottom"])) {
        start["bottom"] <- parameters["bottom"]
        fml <- as.formula(y ~ bottom + (1-bottom)/(1+exp(slope-scale*x)))
      }
      #check if a d parameter exists
      if(!is.na(parameters["top"])) {
        start["top"] <- parameters["top"]
        fml <- as.formula(y ~ top/(1+exp(slope-scale*x)))
      }
    } else {
      #now we know that neither a nor d exists
      fml <- as.formula(y ~ 1/(1+exp(slope-scale*x)))
    }
  }

  #fill the b start point
  if(!is.na(parameters["slope"])) {
    start["slope"] <- parameters["slope"]
  } else {
    start["slope"] <- 5
  }


  dat.nls <- nls(fml, data = df.tmp, start=start)
  data$Fit <- fitted(dat.nls)
  if(!is.null(fixedMaxX)) {
    data$scale = 10/maxX
  }

  #dat.r2.1 <- 1 - dat.nls$m$deviance()/sum((df.tmp$y - mean(df.tmp$y)^2))
  dat.r2.1 <-ch.R2(data$y,data$Fit)
  outList <- list(data = data, fit = dat.nls, r2 = dat.r2.1)

  return(outList)
}
