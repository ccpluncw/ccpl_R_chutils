#' A function to plot a strip chart by groups.
#'
#' This function plots a strip chart by groups.
#' @param data a dataframe.
#' @param xCol a string the specifies the column name in "data" that contains the variable for the xColumn.
#' @param xSortColBy a string the specifies the column name in "data" that contains the variable for to sort the xColumn by.
#' @param grpCols a vector of strings the specifies the column names in "data" that contain the DVs for each group. The order is this vector matters, so sort them in the order you want them graphed (the colors will be relevant). This function depends on a "wide" format. So, each level of each group is in a separate column. DEFAULT = NULL
#' @param grpLgndNames a vector of strings the specifies the names you want each group to be called in the legend. The order is this vector matters, so sort them in the same order as grpCols.
#' @param filename the filename (pdf) to save the figure.  DEFAULT = NULL (file not saved)
#' @param lgndPlacement a string that specified the placement of the legend: "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center". DEFAULT = "bottomright"
#' @param cex1 the relative font size for the y-axis label. DEFAULT = 1.
#' @param cexLegend the relative font size for the legend. DEFAULT = .75.
#' @param ylim the limits of the y axis. DEFAULT = c(-1,1).
#' @param yLab y axis label. DEFAULT = NA.
#' @param pch see par for more info. DEFAULT = 1.
#' @param las see par for more info. DEFAULT = 2.
#' @param parOp the parameter list to enter into par.  DEFAULT = NULL
#' @return .
#' @export
#' @examples ch.stripPlotByGrp (overlapData, "probe", "meanDO", c("g1", "g2"))


ch.stripPlotByGrp <- function (data, xCol, xSortColBy, grpCols = NULL, grpLgndNames = NULL, filename = NULL, lgndPlacement = "topright", cex1 = 1,cexLegend = 0.75, ylim=c(-1,1), yLab = NA, pch=1, las=2, parOp = NULL, ...) {

  if(is.null(parOp)) {
    op <- par(mfrow=c(1,1), mai=c(1,1,1,1), omi=c(1,.75,.25,1), las=2, cex=1, lwd=2, bty='n', xpd = T)
  }

  if(!is.null(grpCols)) {
    nGrps <- length(grpCols)
    hsvCols <- ch.getHSVcolors(nGrps)

    stripchart(data[[grpCols[1]]] ~ reorder(data[[xCol]],data[[xSortColBy]]), col=hsv(hsvCols$h[1], hsvCols$s[1], hsvCols$v[1]), ylab="", vertical=T, ylim=ylim, pch=pch,las=las, ...)

    for (i in 2:nGrps) {
      stripchart(data[[grpCols[i]]] ~ reorder(data[[xCol]],data[[xSortColBy]]), col=hsv(hsvCols$h[i], hsvCols$s[i], hsvCols$v[i]), ylab="", vertical=T, ylim=ylim, pch=pch,las=las, add=T,...)
    }

    if(!is.null(grpLgndNames) & (length(grpLgndNames) == length(grpCols))) {
      lNames <- grpLgndNames
    } else {
      lNames <- grpCols
    }
    legend(lgndPlacement, legend=lNames, pch=1, col=hsv(hsvCols$h,hsvCols$s,hsvCols$v), bty="n", cex=cexLegend, inset = c(-.15,0), x.intersp = .35,y.intersp = 1)
  } else {
    hsvCols <- ch.getHSVcolors(1)
    stripchart(data[[xSortColBy]] ~ reorder(data[[xCol]],data[[xSortColBy]]), col=hsv(hsvCols$h[1], hsvCols$s[1], hsvCols$v[1]), ylab="", vertical=T, ylim=ylim, pch=pch,las=las, ...)
  }

  if(!is.na(yLab)) {
    mtext(side=2,yLab, line=3, cex=cex1)
  }

  if(!is.null(filename)) {
    dev.copy(pdf, filename, width=12, height=7)
    dev.off();
  }

  if(is.null(parOp)) {
    par(op)
    par(xpd = F)
  }

}
