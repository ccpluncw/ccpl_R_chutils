#' A function to summarize a dataframe one grouping variable
#'
#' This function summarizes a dataframe by one grouping variable. It is easier to call than ddply within a function.
#' @param data a dataframe.
#' @param grpCol a string specifying the name of the column in the dataframe containing the grouping variable.
#' @param dvCol a string specifying the name of the column in the dataframe containing the variable you want to summarize.
#' @param newCol a string specifying the name of the new column to be created that contains the summarized data.
#' @param FUN the summarizing function (e.g., mean).
#' @keywords ddply summarize group
#' @return a dataframe
#' @export
#' @importFrom dplyr %>%
#' @examples ch.summariseBy (df, "sn", "RT", mean)

ch.summariseBy <- function(data, grpCol, dvCol, newCol, FUN) {

  res <- as.data.frame(data %>% dplyr::group_by(across(all_of(grpCol))) %>% dplyr::summarise( out = FUN(eval(parse(text=dvCol)))))
  colnames(res)[which(names(res) == "out")] <- newCol
  return(res)
}
