#' New Rounding Function
#'
#' This function rounds numbers to a specified precision to in a particular direction.
#' @param x a vector of numbers.
#' @param accuracy the accuracy of the rounding.
#' @param f the function for rounding: floor, ceiling, round. DEFAULT = ceiling
#' @param center a boolean specifying whether to center the bin label in the ceiling and floor cases. DEFAULT = TRUE
#' @keywords round numbers any
#' @export
#' @examples ch.round_any (x, .1, ceiling)

ch.round_any <-  function(x, accuracy, f=ceiling, center = TRUE){

	out <- f(x/ accuracy) * accuracy

	if(center){
		#subtract half the accuracy to center the bin
		if(grepl("ceiling", deparse1(f), fixed=TRUE)) {
	    out <- out - (accuracy * 0.5)
	  }
		#add half the accuracy to center the bin
	  if(grepl("floor", deparse1(f), fixed=TRUE)) {
	    out <- out + (accuracy * 0.5)
	  }
	}
	return(out)
}

#' New Subdrectory Function
#'
#' This function creates a new subdirectory and moves into it. If the subdirectory already exist, then it just moves into it.
#' @param mainDir the directory under which the subdirectory will exist.
#' @param subDir the new subdirectory.
#' @return a boolean that specifies whether the subDir already exists (TRUE/FALSE).
#' @keywords new directory
#' @export
#' @examples ch.newDir (currentDir, newDir)


ch.newDir <- function (mainDir, subDir) {
		subDirExists <- FALSE
		if (file.exists(file.path(mainDir,subDir))) {
	    setwd(file.path(mainDir, subDir))
			subDirExists <- TRUE
		} else {
	    dir.create(file.path(mainDir, subDir))
	    setwd(file.path(mainDir, subDir))
		}
		return(subDirExists)
}

#' Permutation Function
#'
#' This function creates all the permutations of all the items from 1-n
#' @param n the total number of items.
#' @return a matrix of all the possible permutations of n items
#' @keywords permutations
#' @export
#' @examples ch.permute (4)

ch.permute <- function(n){
    if(n==1){
        return(matrix(1))
    } else {
        sp <- ch.permute(n-1)
        p <- nrow(sp)
        A <- matrix(nrow=n*p,ncol=n)
        for(i in 1:n){
            A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
        }
        return(A)
    }
}

#' Graph function for Values Experiment
#'
#' This function creates a graph of p(hit) (0-1 on y-axis) as a function of an x variable (often overlap). It aldo fits a non-linear function to the data whereby the first point is 1 and the last point is .5. It outputs the fit of the function.
#' @param x the x variable for the x-axis (often overlap).
#' @param y the y variable for the y-axis (often p(hit)).
#' @param useTwoParameterModel A boolean that specifies whether to use a two parameter model.  If this is set to TRUE, then this function will fit a model whereby the rightmost point (overlap = 1.0) is not fixed at p(HVO) = 0.5. DEFAULT = FALSE.
#' @param plotTitle a string with the title of the plot.
#' @param filename a string with the filename of the pdf of the file to be saved.  DEFAULT = NULL; no file saved.
#' @param cex1 a numeric value for cex: the relative size of the text in the graph. cex1 > 1 is bigger; cex1 < 1 is smaller. DEFAULT=1.
#' @param printR2 do you want the r square printed on the graph: TRUE/FALSE. DEFAULT=TRUE.
#' @param yLabel a string with the title of y-axis. DEFAULT='p(hit)'
#' @return a list of the fit, r2, and beta, from the nls .
#' @keywords graph p(hit)
#' @export
#' @examples ch.plot.pHit (x,y)

ch.plot.pHit <- function (x,y, useTwoParameterModel = FALSE, plotTitle = NA, filename=NULL, cex1 = 1, printR2 = T,yLabel="p(HVO)", ...) {

		plot(x, y, main=plotTitle, xlab= expression(paste("", Psi,"(value) Distributional overlap", sep="")), ylab=NA, pch=16, ylim = c(0,1), ...)
		mtext(side=2,yLabel, line=3, cex = cex1)

		pHVOFit <- ch.pHVOfit(x, y, grp = NULL, useTwoParameterModel = useTwoParameterModel)

		if (!is.null(pHVOFit$nlsObject)) {
			abline(a=0.5,b=0,col="grey", lwd=2)
			lines(x, predict(pHVOFit$nlsObject), col="black", lwd=3)
			if (printR2) {
				r2 <- round(pHVOFit$r2, d=2)
				mtext(side=2, bquote(r^2==.(r2)), line=0, at = -.2, cex = .8*cex1)
			}
		}

			if (!is.null(filename)) {
				dev.copy(pdf, filename, width=8, height=8)
				dev.off();
			}

#			return (list(nlsObject = nlsFit, beta = nls.beta, r2 = nlsFit.r2))
			return (pHVOFit)
}

#' Graph function for Values Experiment
#'
#' This function creates a scatterplot (often a function of RT) as a function of an x variable (often overlap). It aldo fits a linear regression to the data. It outputs the fit of the function.
#' @param x the x variable for the x-axis (often overlap).
#' @param y the y variable for the y-axis (often p(hit)).
#' @param plotTitle a string with the title of the plot.
#' @param filename a string with the filename of the pdf of the file to be saved.  DEFAULT = NULL; no file saved.
#' @param cex1 a numeric value for cex: the relative size of the text in the graph. cex1 > 1 is bigger; cex1 < 1 is smaller. DEFAULT=1.
#' @param printR2 do you want the r square printed on the graph: TRUE/FALSE. DEFAULT=TRUE.
#' @param yLabel a string with the title of y-axis. DEFAULT=NA.
#' @param ylimMin a number denoting the minimum of the y-axis. DEFAULT=0.
#' @param ylimMax a number denoting the maximum of the y-axis. DEFAULT=0. If ylimMin == ylimMax, the function determines a pretty y-axis for you.
#' @return the fit the lm .
#' @keywords graph overlap
#' @export
#' @examples ch.plot.lm (x,y)

ch.plot.lm <- function (x,y, plotTitle = NA, filename = NULL, cex1 = 1, printR2 = T, yLabel = NA, ylimMin = 0, ylimMax = 0, xlab= expression(paste("", Psi,"(value) Distributional overlap", sep="")), ...) {
	#	par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,7), las=1, cex=2)

	buffer <- (max(y) - min(y)) * .1
	if (ylimMin == ylimMax) {
		ylimMin <-  min(y) - buffer
		ylimMax <-  max(y) + buffer
	}
		plot(x, y, main=plotTitle, xlab= xlab, ylab=NA, pch=16, ylim=c(ylimMin,ylimMax), ...)
			lmFit <- lm(y ~ x)
			lines(x, fitted(lmFit), col="black", lwd=3)
			#abline(lmFit, col="black", lwd=3)
			if (printR2) {
				r2 <- round(summary(lmFit)$r.squared, d=2)
				mtext(side=2, bquote(r^2==.(r2)), line=0,at=ylimMin - 2 * buffer, cex = .8*cex1)
			}
			mtext(side=2,yLabel, line=3, cex = cex1)

			if (!is.null(filename)) {
				dev.copy(pdf, filename, width=8, height=8)
				dev.off();
			}

			return(lmFit)
}

#' Capitalize first letter Function
#'
#' This function capitalizes the first letter of each word in a string of words.
#' @param s a string of words.
#' @param strict a switch, if true, it ensures that only the first letter of each word is capitalized. Otherwise all remaining letters keep their default capitalization.  DEFAULT=FALSE.
#' @return a string of words with the first letter capitalized
#' @keywords Capitalize
#' @export
#' @examples ch.capwords ("hello world")

ch.capwords <- function(s, strict = FALSE) {
	 cap <- function(s) paste(toupper(substring(s, 1, 1)),
								{s <- substring(s, 2); if(strict) tolower(s) else s},
													 sep = "", collapse = " " )
			sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#' Merge all the files in a directory
#'
#' This function merges all the files in a directory.
#' @param directoryName this is a string that specifies the name of the directory containing the datafiles.
#' @param outputFile this is a string that specifies the name of the merged output file.
#' @param extension this is a string that specifies extensions of the files to be merged. DEFAULT = ".dat"
#' @param sep this is a string that specifies the how the columns in the files are separated. DEFAULT = tab
#' @param header this is a boolean that specifies whether column headers are present. DEFAULT = T
#' @return nothing (a file is written)
#' @keywords merge data directory
#' @export
#' @examples ch.mergeDataInDir ("data", "out.txt")

ch.mergeDataInDir <- function (directoryName, outputFile, extension = ".dat", sep = "\t", header = T, ...) {

	raw.data = list.files(directoryName, full.names=TRUE, pattern=extension, all.files=T)
	merge.all <- do.call(rbind, lapply(raw.data, read.table, sep=sep, header=header, fill=TRUE))
	write.table(merge.all, file=outputFile, quote=F, sep=sep, row.names=F)

}
