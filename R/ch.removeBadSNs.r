#' A function to remove bad subjects' data from a dataframe
#'
#' This function removes bad subjects' data from a dataframe. A filename containing the bad subject IDs are passed to the function.  This file should just be a vector (vertical in file) of IDs
#' @param data a dataframe.
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param badSNfilename the filename containing the IDs of the bad SNs.
#' @keywords remove bad subjects
#' @return a dataframe without the bad subjects' data.
#' @export
#' @examples ch.removeBadSNs (data, "sn", "badSN.txt")


ch.removeBadSNs <- function (data, snCol, badSNfilename) {

  data[[snCol]] <- as.character(data[[snCol]])
  badSN <- scan(badSNfilename, what="a")
  if(length(badSN) > 0) {
    if(badSN[1]=="sn") {
      badSN <- badSN[-1]
    }
  }
  data <- data[!(data[[snCol]] %in% badSN),]

  return(data)
}
