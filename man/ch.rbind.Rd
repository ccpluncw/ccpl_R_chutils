% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.rbind.r
\name{ch.rbind}
\alias{ch.rbind}
\title{A function to append a temporary dataframe to the end of another dataframe}
\usage{
ch.rbind(df.a = NULL, df.tmp)
}
\arguments{
\item{df.a}{the dataframe to be appended to or created from the temporary dataframe.}

\item{df.tmp}{the temporary dataframe that is to be appended to the end of df.a.}
}
\value{
the new appended dataframe
}
\description{
This function appends a temporary dataframe to the end of second dataframe.  The second dataframe may already exist or may be null.  It does not matter.
}
\examples{
ch.rbind (df.1, df.tmp)
}
\keyword{append}
\keyword{dataframe}
\keyword{rbind}
