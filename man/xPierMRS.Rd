% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xPierMRS.r
\name{xPierMRS}
\alias{xPierMRS}
\title{Function to calculate multi-trait rating score from a list of dTarget/sTarget objects}
\usage{
xPierMRS(list_xTarget, cutoff.rank = 150, verbose = TRUE)
}
\arguments{
\item{list_xTarget}{a list of "dTarget"/"sTarget" objects}

\item{cutoff.rank}{the rank cutoff. By default it is 150}

\item{verbose}{logical to indicate whether the messages will be
displayed in the screen. By default, it sets to true for display}
}
\value{
a data frame containing columns 'Target', 'MRS', 'rating' (in the form
of "rating.trait_names") and 'rank' (in the form of
"rank.trait_names").
}
\description{
\code{xPierMRS} is supposed to calculate multi-trait rating score (MRS)
from a list of dTarget/sTarget objects.
}
\note{
none
}
\examples{
RData.location <- "http://galahad.well.ox.ac.uk/bigdata"
\dontrun{
df_MRS <- xPierMRS(ls_xTarget)
}
}
\seealso{
\code{\link{xPierCross}}
}
