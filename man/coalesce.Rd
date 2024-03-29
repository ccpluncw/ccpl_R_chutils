% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/row.r
\name{coalesce}
\alias{coalesce}
\title{A more versatile form of the T-SQL \code{coalesce()} function.}
\usage{
coalesce(..., fun = (function(x, y) if (!is.na(x)) x else y))
}
\arguments{
\item{...}{an arbitrary number of \code{R} objects}

\item{fun}{a two argument function that returns an atomic value}
}
\description{
Little more than a wrapper for \code{\link{vectorize}}, allows for
duplication of SQL coalesce functionality, certain types of if-else
statements, and \code{\link{apply}}/\code{\link{Reduce}} combinations.
}
\details{
Originally written for the row.r package by Craig Varrichio. Included here because the rowr
package was discontinued. I use these functions in my packages
}
\examples{
coalesce(c(NA,1,2))
coalesce(c(NA,1,2),c(3,4,NA))
df<-data.frame(a=c(NA,2,3),b=c(1,2,NA))
coalesce(df$a,df$b)
# Or even just:
coalesce(df)
# Coalesce can actually use any comparison.  For example, instead of non-NA
# values it could find the max in each row:
cbind(EuStockMarkets,Max=coalesce(EuStockMarkets,fun=function (x,y) if (x>y) x else y))
}
