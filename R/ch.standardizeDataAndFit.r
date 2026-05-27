#' Function that standardizes the data and fit of the simulation
#'
#' This function standardizes the data and fit of the simulation
#' @param data a vector of numbers that are the behavioral data.
#' @param fit a vector of numbers that were fit to the data by a model.
#' @return a dataframe with the standardized data and fit
#' @keywords standardize scale
#' @export
#' @examples ch.standardizeDataAndFit (data, fit)


ch.standardizeDataAndFit <- function(data, fit) {
	data.z <- scale(data)[,1]
  sim.z <- scale(fit)[,1]

	df.out <- data.frame(data = data.z, fit = sim.z)
	return(df.out)

}
