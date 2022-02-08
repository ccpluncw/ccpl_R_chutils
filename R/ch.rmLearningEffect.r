
#' A function to remove a learning function from data
#'
#' This function attempts to fit a non-linear, decellerating function to the specifed data.  Then it adds two columns to the dataframe: a column with the predicted datapoints based on the nls fit and a column with the residuals around that fit. The function also returns the nls fit object
#' @param data the dataframe containing the data..
#' @param x a string the specifies the column name in "data" that contains the x variable.
#' @param y a string the specifies the column name in "data" that contains the y variable.
#' @param fitCol a string that specifies the name of the new column that will contain the predicted datapoints.
#' @param resCol a string that specifies the name of the new column that will contain the residual datapoints.
#' @keywords morals learning function remove
#' @return a list containing: data = original datafram with the new columns; nlsFit = the nls fit object.
#' @export
#' @examples ch.moralsRmLearningEffect (data=moralsData,"trial", "RT", "fitRT", "resRT")

ch.rmLearningEffect <- function (data, x, y, fitCol, resCol, a=1, b=-1, c=1) {

		fit.nls <- ch.getLearningEffect(data, x,y, a = a, b = b, c = c)

		data[[resCol]] <- resid(fit.nls)
		data[[fitCol]] <- fitted(fit.nls)

		return(list(data = data, nlsFit = fit.nls))
		
}
