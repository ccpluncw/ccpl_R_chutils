#' A function to assess the learning effect in data
#'
#' This function attempts to fit a non-linear, decelerating function to the specified data.
#' @param data the dataframe.
#' @param x a string the specifies the column name in "data" that contains the x variable.
#' @param y a string the specifies the column name in "data" that contains the y variable.
#' @param a a numeric value for the startpoint specifying the multiplier. DEFAULT = 2000.
#' @param b a numeric value for the startpoint specifying the multiplier in the exponential. DEFAULT = -1.
#' @param c a numeric value for the startpoint specifying the addend in the exponential. DEFAULT = 100.
#' @keywords morals data prep
#' @return a nls fit function.
#' @export
#' @examples ch.getLearningEffect (data=moralsData, trial, RT)

ch.getLearningEffect <- function (data, x, y, a = 2000, b = -1, c = 100, ...) {

  if(min(data[[x]]) == 0) {
    data[[x]] <- data[[x]] + 1
  }

  dfIn <- data.frame (x = data[[x]], y = data[[y]])

  out.nls = NULL
  tryCatch (
    out.nls<-nls(y~a*exp(b*(x-1))+c, data=dfIn, start=list(a=a, b=b, c=c),control =  nls.control(minFactor=1/10000000, maxiter=10000, warnOnly = FALSE)),error = function (w) return (NULL)
  )

  if (is.null(out.nls)) {
  tryCatch (
      out.nls<-nls(y~a*exp(b*(x-1)), data=dfIn, start=list(a=a, b=b), control = nls.control(minFactor=1/10000000, maxiter=10000, warnOnly = FALSE)),
      error = function (w) return (NULL)
    )
  }
  if (is.null(out.nls)) {
    tryCatch (
          out.nls<-nls(y~a + b*(x-1), data=dfIn, start=list(a=2000, b=-1), control = nls.control(minFactor=1/10000000, maxiter=10000, warnOnly = FALSE)),
          error = function (w) return (NULL)
        )
  }
  return(out.nls)
}
