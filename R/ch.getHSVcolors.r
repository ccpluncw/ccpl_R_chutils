
#' A function to create unique hsv colors to be plotted
#'
#' This function creates unique hsv colors to be plotted.
#' @param numColors the number of colors to be returned.
#' @param maxIntensityChanges the maximum number of distinguishable intensity changes. DEFAULT = 8.
#' @param maxHueChanges the maximum number of distinguishable hue changes. DEFAULT = 10.
#' @keywords plot group legend col hsv colors
#' @return an dataframe containing nRow=numColors and h, s, v columns specifying colors.
#' @export
#' @examples ch.getHSVcolors(10)

ch.getHSVcolors <- function (numColors, maxIntensityChanges = 8, maxHueChanges = 10) {

  #determine the number of changes in H, S, and V needed.
  #this depends on the number of levels in the groups and maxLevelThresh
  numGroupHues <- ceiling(numColors/maxIntensityChanges)
  numLevelSats <- ceiling(numGroupHues/maxHueChanges)
  if(numGroupHues > maxHueChanges) {
    numGroupHues <- maxHueChanges
  }
  if (numColors > maxIntensityChanges) {
     numLevelVals <- maxIntensityChanges
  } else {
     numLevelVals <- numColors
  }

  #find the increments in HSV as a function of the number of changes needed
  hsvBounds <- data.frame(minH = 0,maxH = 1, minS = .2, maxS = 1, minV = 0, maxV = .8)
  hsvBounds$incH <- ch.getHSVincrement(numGroupHues, c(hsvBounds$minH, hsvBounds$maxH))
  hsvBounds$incS <- ch.getHSVincrement(numLevelSats,c(hsvBounds$minS, hsvBounds$maxS))
  hsvBounds$incV <- ch.getHSVincrement(numLevelVals,c(hsvBounds$minV, hsvBounds$maxV))

  tmpDF <- list()
  for(j in 1:numColors) {
    tmpDF$h[j] <- hsvBounds$minH + (floor((j-1)/maxIntensityChanges)*hsvBounds$incH)%%1.001
    if(j <= maxIntensityChanges) {
      tmpDF$s[j] <- 0
      tmpDF$v[j] <- hsvBounds$minV + ((j-1)%%maxIntensityChanges)*hsvBounds$incV
    } else {
      tmpDF$s[j] <- hsvBounds$maxS - floor((j-1)/(maxIntensityChanges*maxHueChanges))*hsvBounds$incS
      tmpDF$v[j] <- (1-hsvBounds$maxV) + ((j-1)%%maxIntensityChanges)*hsvBounds$incV
    }

  }

  outDF <- data.frame(tmpDF)

  return(outDF)

}
