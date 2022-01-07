#' A function to append a temporary dataframe to the end of another dataframe
#'
#' This function appends a temporary dataframe to the end of second dataframe.  The second dataframe may already exist or may be null.  It does not matter.
#' @param df.a the dataframe to be appended to or created from the temporary dataframe.
#' @param df.tmp the temporary dataframe that is to be appended to the end of df.a.
#' @keywords append dataframe rbind
#' @return the new appended dataframe
#' @export
#' @examples ch.rbind (df.1, df.tmp)


ch.rbind <- function(df.a = NULL, df.tmp) {

  if (is.null(df.a)) {
    df.a <- df.tmp
  } else {
    df.a <- rbind(df.a, df.tmp)
  }

  return(df.a)
}
