#' A function to create unique color, line type, and point type  for up to three groups to be plotted
#'
#' This function creates unique color, line type, and point type  for up to two groups to be plotted.
#' @param df.grpIndex a dataframe containing a column for each grouping variable with the values containing the combination of those variables that are to be plotted. The last column should be the index number for the model that will be used for plotting.  This dataframe is output by the ch.subsetDFbyGroups() function.
#' @param maxIntensityChanges the maximum number of distinguishable intensity changes. DEFAULT = 8.
#' @param maxHueChanges the maximum number of distinguishable hue changes. DEFAULT = 10.
#' @keywords plot group grouping variables legend lty colors
#' @return this returns a dataframe containing the folloing columns: indexNum (the index number of the dataframe from ch.subsetDFbyGroups()); lty (the line type values for the graph); the group column names; h (the hue for hsv() color of line); s (the saturation for hsv() color of line);v (the intensity for hsv() color of line)
#' @export
#' @examples ch.getPlotLegendVals(df.Index)

ch.getPlotLegendVals <- function (df.grpIndex, maxIntensityChanges = 8, maxHueChanges = 10) {

  grpCols <- names(df.grpIndex)
  #remove the index number column
  grpCols <- grpCols[grpCols !="indexNum"]
  nGrps <- length(grpCols)
  #use only the first two columns
  if(nGrps > 3) {
    grpCols <- grpCols[1:3]
    nGrps <- 3
  }

  #create a dataframe with the group names and number of levels
  df.grpInfo <- NULL
  nLevels <- NULL
  for(i in 1:nGrps){
    df.grpInfo$nLevels[i] <- length(unique(df.grpIndex[,grpCols[i]]))
    df.grpInfo$grpName[i] <- grpCols[i]
  }
  df.grpInfo <- data.frame(df.grpInfo)
  #sort by number of levels
  df.grpInfo <- df.grpInfo[order(df.grpInfo$nLevels, decreasing = TRUE),]

  #create the legend
  pointTypes <- ch.get11diffPchs()
  lineTypes <- ch.get67diffLtys()
  hsvCols <- ch.getHSVcolors(df.grpInfo$nLevels[1], maxIntensityChanges = maxIntensityChanges, maxHueChanges = maxHueChanges)

  df.levelLgnd <- list()
  for(i in 1:nGrps) {
    tmpDF <- list()
    #sort group names so the same legend values are used across experiments
    gLevels <- sort(as.character(unique(df.grpIndex[,as.character(df.grpInfo$grpName[i])])))
    tmpDF[[as.character(df.grpInfo$grpName[i])]] <- gLevels
    for(j in 1:df.grpInfo$nLevels[i]) {
      if(i == 2) {
        #the group with the most levels gets a variation in line type and point type
        tmpDF$lty[j] <- lineTypes[j]
        if(nGrps == 2) {
          tmpDF$pch[j] <- pointTypes[j]
        }
      }
      if(i == 3) {
          tmpDF$pch[j] <- pointTypes[j]
      }
      if(i == 1) {
        tmpDF$h[j] <- hsvCols$h[j]
        tmpDF$s[j] <- hsvCols$s[j]
        tmpDF$v[j] <- hsvCols$v[j]

      }
    }
    tmpDF <- data.frame(tmpDF)
    df.levelLgnd[[as.character(df.grpInfo$grpName[i])]] <- tmpDF
  }

  #merge with df.grpInfo and output
  tmpDF <- merge(df.levelLgnd[[as.character(df.grpInfo$grpName[1])]], df.grpIndex, by=as.character(df.grpInfo$grpName[1]))
  if(nGrps == 1) {
    tmpDF <- tmpDF
    tmpDF$lty <- "solid"
    tmpDF$pch <- 16
  } else {
    for (k in 2:nGrps) {
      tmpDF <- merge(df.levelLgnd[[as.character(df.grpInfo$grpName[k])]], tmpDF, by=as.character(df.grpInfo$grpName[k]))
    }
  }
  outDF <- tmpDF
  outDF$lty <- as.character(outDF$lty)
  return(outDF)

}
