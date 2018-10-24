#' A function to add a legend to a plot
#'
#' This function adds a legend to a plot.
#' @param df.legend a legend dataframe created by ch.getPlotLegendVals().
#' @param grpCols a vector of strings that specifies the names of the column in "df.legend" that contain the grouping variables that are plotted.
#' @param placement the placement of the legend: "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center". DEFAULT = "bottomright"
#' @param horiz a boolean to draw the legend horizontal (vs vertical). DEFAULT = T.
#' @param lwd see par for more info. DEFAULT = 1.
#' @param cex see par for more info. DEFAULT = .5.
#' @param bty see par for more info. DEFAULT = "n"
#' @param includeTitle a boolean to add a title to the legend. DEFAULT = T.
#' @keywords plot add legend
#' @return .
#' @export
#' @examples ch.addLegend(df.legend, c("cond1", "cond2"))

ch.addLegend <- function (df.legend, grpCols, placement="topright", horiz = T, lwd = 1, cexLegend = .5, bty="n", includeTitle = T, ...) {

    op <- par(xpd = T)

    if(length(grpCols) > 1) {
      df.legend$nm <- apply( df.legend[ , grpCols ] , 1 , paste , collapse = "-" )
    } else {
      df.legend$nm <- df.legend[[grpCols]]
    }
    lgndTitle <- NULL
    if(includeTitle) {
      lgndTitle <- paste(grpCols, collapse="-")
    }
    legend(placement, legend=df.legend$nm, title= lgndTitle,col=hsv(df.legend$h,df.legend$s,df.legend$v), lty=as.character(df.legend$lty), lwd=lwd, cex=cexLegend, bty=bty, inset=c(-.3,0), ...)

    par(op)
}
