% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.RTfit.r
\name{ch.RTfit}
\alias{ch.RTfit}
\title{Fits the p(HVO) function for the choice experiment}
\usage{
ch.RTfit(x, y, grp = NULL)
}
\arguments{
\item{x}{the x variable for the x-axis (often overlap).}

\item{y}{the y variable for the y-axis (often p(hit)).}

\item{grp}{the grouping variable that identifies those stimuli whose value are above the reference distribution and those that are below the reference distribution. When this variable is included, then the intercept parameter is allowed to vary for refHVO and refLVO, but the slope is kept constant. DEFAULT = NULL (the grouping variable is ignored)}
}
\value{
a list of the fit, r2 from the nls .
}
\description{
This function fits p(HVO) as a non-linear exponential decay function of an x variable (often overlap). It forces the first point to equal 1 and the last point is .5 on the y-axis. It outputs the fit of the function and the r_square.
}
\examples{
ch.RTfit (x,y, grp)
}
\keyword{fit}
\keyword{p(HVO)}
\keyword{p(hit)}
