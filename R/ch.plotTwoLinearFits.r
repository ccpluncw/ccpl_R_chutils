#' A function to fit and plot two linear function one over the other.
#'
#' This function fits  two linear functions;  plots the two functions on a single graph; and returns a list with the fit statistics.
#' @param data a dataframe.
#' @param xCol a string that specifies the name of the column in "data" that contains the data for the x-axis.
#' @param y1Col a string that specifies the name of the column in "data" that contains the the first DV to be fit.
#' @param y2Col a string that specifies the name of the column in "data" that contains the the second DV to be fit.
#' @param minN an integer indicating the minimum number of trials in each condition that is valid for analysis.  Below that number, the condition will be removed. DEFAULT = NULL.
#' @param y1Label a string that specifies the y-axis label name for the y1Fit. DEFAULT = NA.
#' @param y2Label a string that specifies the y-axis label name for the y2Fit. DEFAULT = NA.
#' @param parOp a list with the par options (see par). If NULL, then the functions default par options will be used. DEFAULT = NULL.
#' @param cex1 sets the default font size. DEFAULT = 1.25.
#' @param cex.topTile sets the default font size of the title at the top of the page. DEFAULT = 1.25.
#' @param printR2 a boolean that determines whether to print the r square on the graph. DEFAULT = FALSE.
#' @param topTitle a string that will be the title at the top of the page. DEFAULT = NULL.
#' @param ylimMin1 a number denoting the minimum of the y-axis for graph 1. DEFAULT=0.
#' @param ylimMax1 a number denoting the maximum of the y-axis for graph 1. DEFAULT=0. If ylimMin1 == ylimMax1, the function determines a
#' @param ylimMin2 a number denoting the minimum of the y-axis for graph 2. DEFAULT=0.
#' @param ylimMax2 a number denoting the maximum of the y-axis for graph 2. DEFAULT=0. If ylimMin2 == ylimMax2, the function determines a
#' @param filename the filename (pdf) to save the output of the graph. DEFAULT = NULL (no graph saved)
#' @keywords linear fit two plots
#' @return a list containing: y1Fit = lm object with the fit of the y1 data; y2Fit = the lm  object with the fit of the y2 data.
#' @export
#' @examples ch.plotTwoLinearFits (data=moralsData,"overlapRound", "resdRT", "correct", c("yes", "no"), filename = "plot.pdf")

ch.plotTwoLinearFits <- function (data, xCol, y1Col, y2Col, minN = NULL, y1Label = NA, y2Label = NA, parOp = NULL, cex1 = 1.25, cex.topTile =1.25, printR2 = T, topTitle = NULL, ylimMin1 = 0, ylimMax1 = 0, ylimMin2 = 0, ylimMax2 = 0, filename = NULL, ...) {

    df.tmp <- as.data.frame(data %>% dplyr::group_by(across(all_of(xCol))) %>% dplyr::summarise(aveY1 = mean(eval(parse(text = y1Col))),
        aveY2 = mean(eval(parse(text = y2Col))),
        N = length(eval(parse(text = y2Col)))
    ) )
    df.tmp <- df.tmp[complete.cases(df.tmp),]

    #remove conditions with too few points
    if (!is.null(minN)) {
      df.tmp <- df.tmp[df.tmp$N > minN,]
    }
    df.tmp <- droplevels(df.tmp)


    #plot d prime and beta by overlap round
    if(is.null(parOp)) {
      op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)
    }

    y1Fit <- ch.plot.lm(df.tmp[[xCol]], df.tmp$aveY1, cex1 = cex1, printR2 = printR2, yLabel  = y1Label, ylimMin=ylimMin1, ylimMax = ylimMax1, ...)
    y2Fit <- ch.plot.lm(df.tmp[[xCol]], df.tmp$aveY2, cex1 = cex1, printR2 = printR2, yLabel  = y2Label, ylimMin=ylimMin2, ylimMax = ylimMax2, ...)
    if(!is.null(topTitle)) {
      mtext(topTitle, outer = TRUE, cex = cex.topTile)
    }
    if (!is.null(filename)) {
        dev.copy(pdf, filename, width=6, height=9)
        dev.off();
    }

    if(is.null(parOp)) {
      par(op)
    }

    outList <- list(y1Fit = y1Fit, y2Fit = y2Fit)
    return (outList)
}
