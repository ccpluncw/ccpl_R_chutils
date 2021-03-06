% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.getHSVcolors.r
\name{ch.getHSVcolors}
\alias{ch.getHSVcolors}
\title{A function to create unique hsv colors to be plotted}
\usage{
ch.getHSVcolors(numColors, maxIntensityChanges = 8, maxHueChanges = 10)
}
\arguments{
\item{numColors}{the number of colors to be returned.}

\item{maxIntensityChanges}{the maximum number of distinguishable intensity changes. DEFAULT = 8.}

\item{maxHueChanges}{the maximum number of distinguishable hue changes. DEFAULT = 10.}
}
\value{
an dataframe containing nRow=numColors and h, s, v columns specifying colors.
}
\description{
This function creates unique hsv colors to be plotted.
}
\examples{
ch.getHSVcolors(10)
}
\keyword{col}
\keyword{colors}
\keyword{group}
\keyword{hsv}
\keyword{legend}
\keyword{plot}
