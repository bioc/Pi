% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClassMethod-Pi.r
\name{eGSEA}
\alias{eGSEA}
\alias{print.eGSEA}
\title{Definition for S3 class \code{eGSEA}}
\usage{
eGSEA(df_summary, leading, full, cross)

\method{print}{eGSEA}(x, ...)
}
\arguments{
\item{df_summary}{a data frame}

\item{leading}{a list}

\item{full}{a list}

\item{cross}{a matrix}

\item{x}{an object of class \code{eGSEA}}

\item{...}{other parameters}
}
\value{
an object of S3 class \code{eGSEA}
}
\description{
\code{eGSEA} mush have following components: df_summary, leading, full,
cross.
}
\examples{
\dontrun{
# Load the library
library(Pi)
}
RData.location <- "http://galahad.well.ox.ac.uk/bigdata"
\dontrun{
eGSEA(df_summary, leading, full, cross)
}
}
\keyword{S3}
\keyword{classes}
