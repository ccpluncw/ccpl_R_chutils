#' A function to create a sequence of string names
#'
#' This function creates a vector of string names with the elements in seqVector appended to the end of stringName. For example String1 String2 String3 ...
#' @param stringName a string that remains constant in the string names.
#' @param seqVector the elements in this vector are appended to the end of stringName.
#' @keywords string sequence append
#' @return the vector of string names
#' @export
#' @examples ch.stringSeq ("string", c(1,2,3))

ch.stringSeq <- function(stringName, seqVector) {

  outVec <- NULL

  for (i in seqVector) {
    xTmp <- paste(stringName, i, sep="")
    if (is.null(outVec)) {
      outVec <- xTmp
    } else {
    outVec <- append(outVec, xTmp)
    }
  }

  return(outVec)
}
