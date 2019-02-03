#' A function to get the df.grpIndex required to create legend values and a legend using ch.getPlotLegendVals () and ch.addLegend.
#'
#' This function creates a dataframe that contains the the factorial combination of grouping columns.
#' @param data the dataframe containing the grouped data.
#' @param grpCols a vector of strings specifying the column names of the grouping variables.
#' @keywords dataframe subset grouping variables df.grpIndex
#' @return a dataframe with the factorial combination of groups in data
#' @export
#' @examples ch.getGrpIndex (myDataframe, c("gender", "condition"))

ch.getGrpIndex <- function (data, grpCols) {

    #find the levels of each grouping variable
    grpLevels <- list()

    for (i in grpCols) {
      grpLevels[[i]] <- unique(as.character(data[[i]]))
    }
    #get the factorial combination of all the levels of every grouping variable
    grpSubsetConds <- expand.grid(grpLevels, stringsAsFactors=F)

    #### use grpSubsetConds to create evaluation conditions for the for loop to subset data
    dfIndex <- NULL
    for(i in 1:nrow(grpSubsetConds)) {
      tmpCond <- NULL
      for (j in 1:ncol(grpSubsetConds)) {
        col <- colnames(grpSubsetConds[j])
        val <- as.vector(grpSubsetConds[i,j])

        if (!is.null(tmpCond)) {
          tmpCond <- bquote(.(tmpCond)  & .(as.name(col)) == .(val))
        } else {
          tmpCond <-  bquote(.(as.name(col)) == .(val))
        }
      }

      #create index for outList
      tmpDF <- data.frame(grpSubsetConds[i,], indexNum = i)
      if (length(grpCols) == 1) {
        colnames(tmpDF)[1] <- col
      }
      dfIndex <- ch.rbind(dfIndex, tmpDF)
    }
    return(dfIndex)
}
