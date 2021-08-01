#' a function to return a p(Hit) model that can be evaluated (i.e, an expression) from an nls object
#'
#' This function returns a p(Hit) model that can be evaluated (i.e, an expression) from an nls object
#' @param pHitFit an nls object - specifically from ch.pHVOfit().
#' @return expression with of the best fitting model (with parameters and predictor variables)
#' @keywords nls object p(Hit) model eval expression
#' @export
#' @examples ch.getPhitModel (nlsObject)
#' @examples model <- ch.getPhitModel (nlsObject)
#' @examples with(data, eval(model)

ch.getPhitModel <- function (pHitFit, yLab = "p(Hit)", xLab = expression(paste("", Psi,"(value) Distributional overlap", sep=""))) {

  if(is.null(pHitFit)) {
    outList <- list(model = NA, vars = NA, xLab = NA, yLab = NA, modelType = "pHit")

  } else {
    var <- all.vars(formula(pHitFit)[[3]])[1]

    beta <- as.vector(coef(pHitFit)["b"])
    alpha <- ifelse (is.na(as.vector(coef(pHitFit)["a"])), 0.5, as.vector(coef(pHitFit)["a"]))

    model <- bquote((1-.(alpha))*(1-(.(as.name(var))^.(beta) ))+.(alpha))

    outList <- list(model = model, vars = var, xLab = xLab, yLab = yLab, modelType = "pHit")

  }

  return(outList)
}
