#' A function to plot the learning effect in data
#'
#' This function plots the fit of a non-linear, decellerating function to the specifed data.
#' @param x a vector containing the x variable.
#' @param y a vector containing the y variable.
#' @param fit a vector containing the variable with the best fit to the datapoints. If fit==NULL, then no function will be fit.  If length(fit) == 2, then abline will be fit.  if length(fit) == length(x), fit will be the best predicted datapoints for each x.
#' @param plotTitle a string with the title of the plot.
#' @param filename a string with the filename of the pdf of the file to be saved.  DEFAULT = NULL; no file saved.
#' @param cex1 a numeric value for cex: the relative size of the text in the graph. cex1 > 1 is bigger; cex1 < 1 is smaller. DEFAULT=1.
#' @param yLabel a string with the title of y-axis. DEFAULT=NA.
#' @param xLabel a string with the title of x-axis. DEFAULT="Trial Number".
#' @param ylimMin a number denoting the minimum of the y-axis. DEFAULT=0.
#' @param ylimMax a number denoting the maximum of the y-axis. DEFAULT=0. If ylimMin == ylimMax, the function determines a pretty y-axis for you.
#' @keywords plot learning function
#' @return nothing.
#' @export
#' @examples ch.plot.learning (trial, RT, fit, "myplot.pdf")


ch.plot.learning <- function (x, y, fit = NULL, plotTitle = NA, filename = NULL, cex1 = 1, yLabel = NA, xLabel = "Trial Number", ylimMin = 0, ylimMax = 0, ...) {

    if (ylimMin == ylimMax) {
      buffer <- (max(y) - min(y)) * .1
      ylimMin <-  min(y) - buffer
      ylimMax <-  max(y) + buffer
    }

    plot(x, y, main=plotTitle, xlab=xLabel, ylab=NA, pch=16, ylim=c(ylimMin,ylimMax), bty="n", col="grey75")
      axis(side=1, lwd=2)
      axis(side=2, lwd=2)

    if (!is.null(fit)) {
      if (length(fit) == 2) {
        abline(fit[1],fit[2], col = "black")
      } else {
        if (length(fit) == length(x)) {
          points(x,fit, col="black", pch=20)
        }
      }
    }

    mtext(side=2,yLabel, line=3, cex=cex1)

    if (!is.null(filename)) {
      dev.copy(pdf, filename, width=12, height=9)
      dev.off();
    }

}
