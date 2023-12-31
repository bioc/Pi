% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClassMethod-Pi.r
\name{ls_eTerm}
\alias{ls_eTerm}
\alias{print.ls_eTerm}
\title{Definition for S3 class \code{ls_eTerm}}
\usage{
ls_eTerm(df, mat, gp)

\method{print}{ls_eTerm}(x, ...)
}
\arguments{
\item{df}{a data frame}

\item{mat}{a matrix}

\item{gp}{a ggplot object}

\item{x}{an object of class \code{ls_eTerm}}

\item{...}{other parameters}
}
\value{
an object of S3 class \code{ls_eTerm}
}
\description{
\code{ls_eTerm} has 3 components: df, mat and gp.
}
\examples{
RData.location <- "http://galahad.well.ox.ac.uk/bigdata"
\dontrun{
ls_eTerm(df, mat, gp)
}
}
\keyword{S3}
\keyword{classes}
