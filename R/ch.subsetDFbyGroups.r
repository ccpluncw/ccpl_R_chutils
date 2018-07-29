#' A function to subset a dataframe by the factorial combination of grouping columns
#'
#' This function subsets a dataframe by the factorial combination of grouping columns.  It outputs a list with each subsetted dataframe
#' @param data the dataframe to be subsetted.
#' @param grpCols a vector of strings specifying the column names of the grouping variables.
#' @keywords dataframe subset grouping variables
#' @return a list with all the subsetted dataframes
#' @export
#' @examples ch.subsetDFbyGroups (myDataframe, c("gender", "condition"))

ch.subsetDFbyGroups <- function (data, grpCols) {

    outList <- list()
    #find the levels of each grouping variable
    grpLevels <- list()

    for (i in grpCols) {
      data[[i]] <- as.character(data[[i]])
      grpLevels[[i]] <- unique(data[[i]])
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
      condit <- as.call(tmpCond)

      #create index for outList
      tmpDF <- data.frame(grpSubsetConds[i,], indexNum = i)
      dfIndex <- ch.rbind(dfIndex, tmpDF)
      #subset the data based on the expression in condit
      outList[[i]] <- subset(data, eval(condit))
    }
    outList[["dfIndex"]] <- dfIndex
    return(outList)
}
