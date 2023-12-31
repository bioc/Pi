% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xMLrandomforest.r
\name{xMLrandomforest}
\alias{xMLrandomforest}
\title{Function to integrate predictor matrix in a supervised manner via machine learning algorithm random forest.}
\usage{
xMLrandomforest(
list_pNode = NULL,
df_predictor = NULL,
GSP,
GSN,
nfold = 3,
nrepeat = 10,
seed = 825,
mtry = NULL,
ntree = 1000,
fold.aggregateBy = c("logistic", "Ztransform", "fishers",
"orderStatistic"),
verbose = TRUE,
RData.location = "http://galahad.well.ox.ac.uk/bigdata",
guid = NULL,
...
)
}
\arguments{
\item{list_pNode}{a list of "pNode" objects or a "pNode" object}

\item{df_predictor}{a data frame containing genes (in rows) and
predictors (in columns), with their predictive scores inside it. This
data frame must has gene symbols as row names}

\item{GSP}{a vector containing Gold Standard Positive (GSP)}

\item{GSN}{a vector containing Gold Standard Negative (GSN)}

\item{nfold}{an integer specifying the number of folds for cross
validataion. Per fold creates balanced splits of the data preserving
the overall distribution for each class (GSP and GSN), therefore
generating balanced cross-vallidation train sets and testing sets. By
default, it is 3 meaning 3-fold cross validation}

\item{nrepeat}{an integer specifying the number of repeats for cross
validataion. By default, it is 10 indicating the cross-validation
repeated 10 times}

\item{seed}{an integer specifying the seed}

\item{mtry}{an integer specifying the number of predictors randomly
sampled as candidates at each split. If NULL, it will be tuned by
`randomForest::tuneRF`, with starting value as sqrt(p) where p is the
number of predictors. The minimum value is 3}

\item{ntree}{an integer specifying the number of trees to grow. By
default, it sets to 2000}

\item{fold.aggregateBy}{the aggregate method used to aggregate results
from k-fold cross validataion. It can be either "orderStatistic" for
the method based on the order statistics of p-values, or "fishers" for
Fisher's method, "Ztransform" for Z-transform method, "logistic" for
the logistic method. Without loss of generality, the Z-transform method
does well in problems where evidence against the combined null is
spread widely (equal footings) or when the total evidence is weak;
Fisher's method does best in problems where the evidence is
concentrated in a relatively small fraction of the individual tests or
when the evidence is at least moderately strong; the logistic method
provides a compromise between these two. Notably, the aggregate methods
'Ztransform' and 'logistic' are preferred here}

\item{verbose}{logical to indicate whether the messages will be
displayed in the screen. By default, it sets to TRUE for display}

\item{RData.location}{the characters to tell the location of built-in
RData files. See \code{\link{xRDataLoader}} for details}

\item{guid}{a valid (5-character) Global Unique IDentifier for an OSF
project. See \code{\link{xRDataLoader}} for details}

\item{...}{additional parameters. Please refer to
'randomForest::randomForest' for the complete list.}
}
\value{
an object of class "sTarget", a list with following components:
\itemize{
\item{\code{model}: a list of models, results from per-fold train set}
\item{\code{priority}: a data frame of nGene X 5 containing gene
priority information, where nGene is the number of genes in the input
data frame, and the 5 columns are "GS" (either 'GSP', or 'GSN', or
'NEW'), "name" (gene names), "rank" (priority rank), "rating" (the
5-star priority score/rating), and "description" (gene description)}
\item{\code{predictor}: a data frame, which is the same as the input
data frame but inserting an additional column 'GS' in the first column}
\item{\code{pred2fold}: a list of data frame, results from per-fold
test set}
\item{\code{prob2fold}: a data frame of nGene X 2+nfold containing the
probability of being GSP, where nGene is the number of genes in the
input data frame, nfold is the number of folds for cross validataion,
and the first two columns are "GS" (either 'GSP', or 'GSN', or 'NEW'),
"name" (gene names), and the rest columns storing the per-fold
probability of being GSP}
\item{\code{importance2fold}: a data frame of nPredictor X 4+nfold
containing the predictor importance info per fold, where nPredictor is
the number of predictors, nfold is the number of folds for cross
validataion, and the first 4 columns are "median" (the median of the
importance across folds), "mad" (the median of absolute deviation of
the importance across folds), "min" (the minimum of the importance
across folds), "max" (the maximum of the importance across folds), and
the rest columns storing the per-fold importance}
\item{\code{roc2fold}: a data frame of 1+nPredictor X 4+nfold
containing the supervised/predictor ROC info (AUC values), where
nPredictor is the number of predictors, nfold is the number of folds
for cross validataion, and the first 4 columns are "median" (the median
of the AUC values across folds), "mad" (the median of absolute
deviation of the AUC values across folds), "min" (the minimum of the
AUC values across folds), "max" (the maximum of the AUC values across
folds), and the rest columns storing the per-fold AUC values}
\item{\code{fmax2fold}: a data frame of 1+nPredictor X 4+nfold
containing the supervised/predictor PR info (F-max values), where
nPredictor is the number of predictors, nfold is the number of folds
for cross validataion, and the first 4 columns are "median" (the median
of the F-max values across folds), "mad" (the median of absolute
deviation of the F-max values across folds), "min" (the minimum of the
F-max values across folds), "max" (the maximum of the F-max values
across folds), and the rest columns storing the per-fold F-max values}
\item{\code{importance}: a data frame of nPredictor X 2 containing the
predictor importance info, where nPredictor is the number of
predictors, two columns for two types ("MeanDecreaseAccuracy" and
"MeanDecreaseGini") of predictor importance measures.
"MeanDecreaseAccuracy" sees how worse the model performs without each
predictor (a high decrease in accuracy would be expected for very
informative predictors), while "MeanDecreaseGini" measures how pure the
nodes are at the end of the tree (a high score means the predictor was
important if each predictor is taken out)}
\item{\code{performance}: a data frame of 1+nPredictor X 2 containing
the supervised/predictor performance info predictor performance info,
where nPredictor is the number of predictors, two columns are "ROC"
(AUC values) and "Fmax" (F-max values)}
\item{\code{evidence}: an object of the class "eTarget", a list with
following components "evidence" and "metag"}
\item{\code{list_pNode}: a list of "pNode" objects}
}
}
\description{
\code{xMLrandomforest} is supposed to integrate predictor matrix in a
supervised manner via machine learning algorithm random forest. It
requires three inputs: 1) Gold Standard Positive (GSP) targets; 2) Gold
Standard Negative (GSN) targets; 3) a predictor matrix containing genes
in rows and predictors in columns, with their predictive scores inside
it. It returns an object of class 'sTarget'.
}
\note{
none
}
\examples{
RData.location <- "http://galahad.well.ox.ac.uk/bigdata"
\dontrun{
sTarget <- xMLrandomforest(df_prediction, GSP, GSN)
}
}
\seealso{
\code{\link{xPierMatrix}}, \code{\link{xSparseMatrix}},
\code{\link{xPredictROCR}}, \code{\link{xPredictCompare}},
\code{\link{xSymbol2GeneID}}
}
