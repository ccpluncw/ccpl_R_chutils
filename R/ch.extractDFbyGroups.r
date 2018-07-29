#' A function to extract a subsetted dataframe returned the function ch.subsetDFbyGroups()
#'
#' This function extracts a specific dataframe the list of dataframes that is returned from the function ch.subsetDFbyGroups().  To identify the dataframe to be extracted, you create a one line dataframe that contains only the grouping variables used as an argument in ch.subsetDFbyGroups().  However, the first (and only) row should contain the value of each grouping variable that you want the dataframe to contain.  For example, grpSubsetCond <- data.frame(gender = "Female", condition = "math").
#' @param subsettedDFlist the list of subsetted dataframes returned by ch.subsetDFbyGroups().
#' @param grpCols a single rowed dataframe that contains only the grouping variables used as an argument in ch.subsetDFbyGroups().  The first (and only) row should contain the value of each grouping variable that you want the dataframe to contain. For example, grpSubsetCond <- data.frame(gender = "Female", condition = "math").
#' @keywords dataframe subset grouping variables extract
#' @return a dataframe that is identified by grpSubsetCond
#' @export
#' @examples ch.subsetDFbyGroups (outList, data.frame(gender = "Female", condition = "math"))

ch.extractDFbyGroups <- function (subsettedDFlist, grpSubsetCond) {

    indexDF <- subsettedDFlist$IndexOfDFs

    tmpCond <- NULL
    for (j in 1:ncol(grpSubsetCond)) {
      col <- colnames(grpSubsetCond[j])
      val <- as.vector(grpSubsetCond[1,j])

      if (!is.null(tmpCond)) {
        tmpCond <- bquote(.(tmpCond)  & indexDF[[.(col)]] == .(val))
      } else {
        tmpCond <-  bquote(indexDF[[.(col)]] == .(val))
      }
    }
    condit <- as.call(tmpCond)

    outIndex <- indexDF[eval(condit),"indexNum"]

    return(subsettedDFlist[[outIndex]])
}
