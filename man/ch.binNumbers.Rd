% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.binNumbers.r
\name{ch.binNumbers}
\alias{ch.binNumbers}
\title{A function bin a vector of numbers}
\usage{
ch.binNumbers(data, numBins, centerLabels = TRUE)
}
\arguments{
\item{data}{A numeric vector.}

\item{numBins}{The number of bins you want the vector divided into.}

\item{centerLabels}{A boolean, if TRUE will create labels that are the center of the bin. If FALSE, the labels will be the max number in the bin. DEFAULT = TRUE
'}
}
\value{
A vector of bins that match to the rows of "data".
}
\description{
This function creates bins for a vector of numbers.
}
\examples{
ch.binNumbers (rnorm(100, 10, 2), 10)
}
\keyword{bin}
\keyword{vector}
