
#' A function to bin quantities into categories.
#'
#' This function bins quantities into categories.
#' @param data a dataframe.
#' @param quantCol a string the specifies the column name in "data" that contains the variable with the quantity.  This will be "p1Quant" or "p2Quant" if the items were processed with ch.moralsEquateItemStringsForNumberQuantDataset().
#' @param catNameCol a string the specifies the new column name in "data" that will be created to hold the new category names.
#' @param quantValueCuts a vector of numbers that specifies the cutoff values for each quantity.  The first category is <= to the first cutoff value; each successive category (c1, c2, etc) is c1 < X <= c2; the final category is greater than the last number in the vector.
#' @return the dataframe (data) with the new category variable added.
#' @export
#' @examples ch.numberVectorToGrps (itemAnalDat, "p1Quant", c(1,9,40))

ch.numberVectorToGrps <- function (data, quantCol, catNameCol, quantValueCuts) {

	numCuts <- length(quantValueCuts)
	quantValueCuts <- sort(quantValueCuts)

	data[[catNameCol]] <- NA

	#get min and max for name
	minG <- min(data[[quantCol]][ data[[quantCol]] <= quantValueCuts[1] ], na.rm=T)
	maxG <- max(data[[quantCol]][ data[[quantCol]] <= quantValueCuts[1] ], na.rm=T)
	if(minG==maxG) {
		nameG <- minG
	} else {
		nameG <- paste(minG,"-", maxG, sep="")
	}

	data[[catNameCol]] [ data[[quantCol]] <= quantValueCuts[1] ] <- nameG
	for(i in 2:numCuts) {

		#get min and max for name
		minG <- min(data[[quantCol]][ data[[quantCol]] > quantValueCuts[i-1] & data[[quantCol]] <= quantValueCuts[i] ], na.rm=T)
		maxG <- max(data[[quantCol]][ data[[quantCol]] > quantValueCuts[i-1] & data[[quantCol]] <= quantValueCuts[i] ], na.rm=T)
		if(minG==maxG) {
			nameG <- minG
		} else {
			nameG <- paste(minG,"-", maxG, sep="")
		}

		data[[catNameCol]] [ data[[quantCol]] > quantValueCuts[i-1] & data[[quantCol]] <= quantValueCuts[i] ] <- nameG
	}

	#get min and max for name
	minG <- min(data[[quantCol]][ data[[quantCol]] > quantValueCuts[numCuts] ], na.rm=T)
	maxG <- max(data[[quantCol]][ data[[quantCol]] > quantValueCuts[numCuts] ], na.rm=T)
	if(minG==maxG) {
		nameG <- minG
	} else {
		nameG <- paste(minG,"-", maxG, sep="")
	}

	data[[catNameCol]] [ data[[quantCol]] > quantValueCuts[numCuts] ] <- nameG

	return(data)
}
