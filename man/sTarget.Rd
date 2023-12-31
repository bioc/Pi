% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClassMethod-Pi.r
\name{sTarget}
\alias{sTarget}
\alias{print.sTarget}
\title{Definition for S3 class \code{sTarget}}
\usage{
sTarget(priority, predictor, performance, importance, evidence)

\method{print}{sTarget}(x, ...)
}
\arguments{
\item{priority}{a data frame}

\item{predictor}{a data frame}

\item{performance}{a data frame}

\item{importance}{a data frame}

\item{evidence}{an 'eTarget' object}

\item{x}{an object of class \code{sTarget}}

\item{...}{other parameters}
}
\value{
an object of S3 class \code{sTarget}
}
\description{
\code{sTarget} mush have following components: priority, predictor,
performance, importance, evidence.
}
\examples{
\dontrun{
# Load the library
library(Pi)
}
RData.location <- "http://galahad.well.ox.ac.uk/bigdata"
\dontrun{
sTarget(priority, predictor, performance, importance, evidence)
}
}
\keyword{S3}
\keyword{classes}
