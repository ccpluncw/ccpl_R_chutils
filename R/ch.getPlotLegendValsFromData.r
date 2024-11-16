#' A function to create unique color, line type, and point type for up to two groups to be plotted from a dataframe
#'
#' This function creates unique color, line type, and point type for up to two groups to be plotted from a dataframe
#' @param data the dataframe to be plotted.
#' @param grpCols a vector of strings specifying the column names of the grouping variables.
#' @keywords dataframe legend grouping variables
#' @return a list with all the subsetted dataframes
#' @export
#' @examples ch.getPlotLegendValsFromData (myDataframe, c("gender", "condition"))

ch.getPlotLegendValsFromData <- function (data, grpCols, maxIntensityChanges = 8, maxHueChanges = 10) {

#get conditions and number of conditions
  if(!is.null(grpCols)) {
    if(length(grpCols) > 1) {
      conds <- unique(data[, grpCols])
      conds.n <- nrow(conds)
      df.grpIdx <- conds
      df.grpIdx$indexNum <- seq(1,conds.n, 1)
    } else {
      conds <- unique(data[[grpCols]])
      conds.n <- length(conds)
      df.grpIdx <- data.frame(cond = conds, indexNum = seq(1,conds.n, 1))
      colnames(df.grpIdx)[which(names(df.grpIdx) == "cond")] <- grpCols[1]
    }
    #create plot info and legend info
    df.legend <- chutils::ch.getPlotLegendVals(df.grpIdx, maxIntensityChanges = maxIntensityChanges, maxHueChanges = maxHueChanges)
  } else {
    conds.n <- 1
    df.grpIdx <- data.frame(cond = "all", indexNum = c(1))
    df.legend <- chutils::ch.getPlotLegendVals(df.grpIdx, maxIntensityChanges = maxIntensityChanges, maxHueChanges = maxHueChanges)
  }

  return(df.legend)
}
