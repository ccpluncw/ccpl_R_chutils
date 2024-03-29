% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.getNormalizedResiduals.r
\name{ch.getNormalizedResiduals}
\alias{ch.getNormalizedResiduals}
\title{A function calculate the normalized residuals (resid/sd) from a model fit}
\usage{
ch.getNormalizedResiduals(y, yFit, yVar)
}
\arguments{
\item{y}{A numeric vector containing the empirical Y from the data.}

\item{yFit}{A numeric vector containing the fit Y from the model.}

\item{yVar}{A numeric vector containing the variance of each row in the Y vector.
'}
}
\value{
a vectpr containing the normalized residuals.
}
\description{
This function calculates the normalized residuals from a from a model fit.  The empirical Y, y variance, and the fit Y are input.
}
\examples{
ch.getNormalizedResiduals (myY, fitY, yVaraince)
}
\keyword{normalized}
\keyword{residuals}
