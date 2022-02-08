#' A function to extract the rows in FileA that contain the elements in FileB.
#'
#' This function extracts the rows in FileA that contain the elements in FileB. FileA is a dataframe and FileB is a vector.
#' @param datafile a string specifying the name of the file containing a dataframe (i.e., FileA).
#' @param itemColName a string specifying the column name in the datafile that contains the items to be matched.
#' @param colItemFile a file containing a dataframe (i.e., FileA).
#' @param datafile a string specifying the name of the file containing the vector of elements used to identify the rows to extract from the datafile (i.e., FileB).
#' @param saveFile a string specifying the name of the file to save the output. DEFAULT = NULL (no output).
#' @keywords extract rows dataframe from elements
#' @return a dataframe containing the extracted rows.
#' @export
#' @examples ch.getRowsByColItemsFiles ("myfile.txt", "prompt", "myItems.txt", "myOutput.txt")

ch.getRowsByColItemsFiles <- function (datafile, itemColName, colItemFile, saveFile = NULL) {
  data <- read.table(datafile, header=T, sep="\t", quote="\"")

  colItems <- read.table(colItemFile, header=F, sep="\t", quote="\"")
  colItems <- colItems[,1]
  colItems <- factor(colItems)

  dataOut <- data[(data[[itemColName]] %in% colItems),]

  if (!is.null(saveFile)) {
    write.table(dataOut, file=saveFile, quote=F, sep="\t", row.names = F, append=F)
  }
  return (dataOut)
}
