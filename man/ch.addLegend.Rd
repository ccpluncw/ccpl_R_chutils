% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.addLegend.r
\name{ch.addLegend}
\alias{ch.addLegend}
\title{A function to add a legend to a plot}
\usage{
ch.addLegend(
  df.legend,
  grpCols,
  placement = "topright",
  horiz = T,
  lwd = 1,
  cexLegend = 0.5,
  bty = "n",
  includeTitle = T,
  inset = c(-0.3, 0),
  ...
)
}
\arguments{
\item{df.legend}{a legend dataframe created by ch.getPlotLegendVals().}

\item{grpCols}{a vector of strings that specifies the names of the column in "df.legend" that contain the grouping variables that are plotted.}

\item{placement}{the placement of the legend: "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center". DEFAULT = "topright"}

\item{horiz}{a boolean to draw the legend horizontal (vs vertical). DEFAULT = T.}

\item{lwd}{see par for more info. DEFAULT = 1.}

\item{bty}{see par for more info. DEFAULT = "n"}

\item{includeTitle}{a boolean to add a title to the legend. DEFAULT = T.}

\item{cex}{see par for more info. DEFAULT = .5.}
}
\value{
.
}
\description{
This function adds a legend to a plot.
}
\examples{
ch.addLegend(df.legend, c("cond1", "cond2"))
}
\keyword{add}
\keyword{legend}
\keyword{plot}
