% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.plot.learning.r
\name{ch.plot.learning}
\alias{ch.plot.learning}
\title{A function to plot the learning effect in data}
\usage{
ch.plot.learning(
  x,
  y,
  fit = NULL,
  plotTitle = NA,
  filename = NULL,
  cex1 = 1,
  yLabel = NA,
  xLabel = "Trial Number",
  ylimMin = 0,
  ylimMax = 0,
  ...
)
}
\arguments{
\item{x}{a vector containing the x variable.}

\item{y}{a vector containing the y variable.}

\item{fit}{a vector containing the variable with the best fit to the datapoints. If fit==NULL, then no function will be fit.  If length(fit) == 2, then abline will be fit.  if length(fit) == length(x), fit will be the best predicted datapoints for each x.}

\item{plotTitle}{a string with the title of the plot.}

\item{filename}{a string with the filename of the pdf of the file to be saved.  DEFAULT = NULL; no file saved.}

\item{cex1}{a numeric value for cex: the relative size of the text in the graph. cex1 > 1 is bigger; cex1 < 1 is smaller. DEFAULT=1.}

\item{yLabel}{a string with the title of y-axis. DEFAULT=NA.}

\item{xLabel}{a string with the title of x-axis. DEFAULT="Trial Number".}

\item{ylimMin}{a number denoting the minimum of the y-axis. DEFAULT=0.}

\item{ylimMax}{a number denoting the maximum of the y-axis. DEFAULT=0. If ylimMin == ylimMax, the function determines a pretty y-axis for you.}
}
\value{
nothing.
}
\description{
This function plots the fit of a non-linear, decellerating function to the specifed data.
}
\examples{
ch.plot.learning (trial, RT, fit, "myplot.pdf")
}
\keyword{function}
\keyword{learning}
\keyword{plot}
