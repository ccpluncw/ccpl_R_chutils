#' A function to create unique color and line type values for up to two groups to be plotted
#'
#' This function creates unique color and line type values for groups to be plotted.
#' @param df.grpIndex a dataframe containing a column for each grouping variable with the values containing the combination of those variables that are to be plotted. The last column should be the index number for the model that will be used for plotting.  This dataframe is output by the XXX function.
#' @param groupsToPlot a vector of up to two strings containing the column names in df.grpIndex that the legend will be created for.
#' @keywords plot group grouping variables legend lty colors
#' @return .
#' @export
#' @examples ch.getPlotLegendVals(df.Index)

ch.getPlotLegendVals <- function (df.grpIndex) {

    grpCols <- names(df.grpIndex)
    nGrps <- length(grpCols)
    #use only the first two columns
    if(nGrps > 2) {
      grpCols <- grpCols[1:2]
      nGrps <- 2
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
    df.grpInfo <- df.grpInfo[order(df.grpInfo$nLevels),]

    #create the legend
    lineTypes <- ch.get67diffLtys()
    hsvCols <- ch.getHSVcolors(df.grpInfo$nLevels[1])

    df.levelLgnd <- list()
    for(i in 1:nGrps) {
      tmpDF <- list()
      gLevels <- unique(df.grpIndex[,as.character(df.grpInfo$grpName[i])])
      tmpDF[[as.character(df.grpInfo$grpName[i])]] <- gLevels
      for(j in 1:df.grpInfo$nLevels[i]) {
        if(i == 2) {
          #the group with the most levels gets a variation in line type
          tmpDF$lty[j] <- lineTypes[j]
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
    outDF <- merge(df.levelLgnd[[as.character(df.grpInfo$grpName[2])]], tmpDF, by=as.character(df.grpInfo$grpName[2]))

    return(outDF)

}
