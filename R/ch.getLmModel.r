#' a function to return an LM model that can be evaluated (i.e, an expression) from an lm object
#'
#' This function returns an LM model that can be evaluated (i.e, an expression) from an lm object
#' @param linearFit an lm object.
#' @return expression with of the best fitting model (with parameters and predictor variables)
#' @keywords lm object model eval expression
#' @export
#' @examples ch.getLmModel (lmObject)
#' @examples model <- ch.getLmModel (lmObject)
#' @examples with(data, eval(model)

ch.getLmModel <- function (linearFit, yLab = NULL, xLab = expression(paste("", Psi,"(value) Distributional overlap", sep=""))) {

  numVars <- length(coef(linearFit))
  vars <- names(coef(linearFit))
  outVars <- unique(unlist(strsplit(vars, ":")))

  if (vars[1] == "(Intercept)") {
    model <- bquote(.(as.vector(coef(linearFit)[1])) )
    outVars <- outVars[-1]
  } else {
    model <- bquote(.(as.vector(coef(linearFit)[1])) * .(as.name(vars[1])) )
  }

  for(i in 2:numVars) {
    if(grepl(":", vars[i]) == T) {
      tmpV <- strsplit(vars[i], ":")
      for (j in 1:length(tmpV[[1]])) {
        if (j == 1) {
          tmpTerm <- bquote (.(as.vector(coef(linearFit)[i])) * .(as.name(tmpV[[1]][j])) )

        } else {
          tmpTerm <- bquote(.(tmpTerm) * .(as.name(tmpV[[1]][j])) )
        }
      }
      model <- bquote(.(model) + .(tmpTerm) )

    } else {
      model <- bquote(.(model) + .(as.vector(coef(linearFit)[i])) * .(as.name(vars[i])))
    }
  }

  outList <- list(model = model, vars = outVars, xLab = xLab, yLab = yLab, modelType = "lm")
  return(outList)
}
