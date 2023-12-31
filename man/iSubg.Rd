% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClassMethod-Pi.r
\name{iSubg}
\alias{iSubg}
\alias{print.iSubg}
\title{Definition for S3 class \code{iSubg}}
\usage{
iSubg(g, ls_subg)

\method{print}{iSubg}(x, ...)
}
\arguments{
\item{g}{an igraph object}

\item{ls_subg}{a list of igraph objects}

\item{x}{an object of class \code{iSubg}}

\item{...}{other parameters}
}
\value{
an object of S3 class \code{iSubg}
}
\description{
\code{iSubg} has 2 components: g, ls_subg.
}
\examples{
RData.location <- "http://galahad.well.ox.ac.uk/bigdata"
\dontrun{
iSubg(g, ls_subg)
}
}
\keyword{S3}
\keyword{classes}
