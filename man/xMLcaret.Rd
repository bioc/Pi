% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xMLcaret.r
\name{xMLcaret}
\alias{xMLcaret}
\title{Function to integrate predictor matrix in a supervised manner via machine learning algorithms using caret.}
\usage{
xMLcaret(
list_pNode = NULL,
df_predictor = NULL,
GSP,
GSN,
method = c("gbm", "svmRadial", "rda", "knn", "pls", "nnet", "rf",
"myrf", "cforest",
"glmnet", "glm", "bayesglm", "LogitBoost", "xgbLinear", "xgbTree"),
nfold = 3,
nrepeat = 10,
seed = 825,
aggregateBy = c("none", "logistic", "Ztransform", "fishers",
"orderStatistic"),
verbose = TRUE,
RData.location = "http://galahad.well.ox.ac.uk/bigdata",
guid = NULL
)
}
\arguments{
\item{list_pNode}{a list of "pNode" objects or a "pNode" object}

\item{df_predictor}{a data frame containing genes (in rows) and
predictors (in columns), with their predictive scores inside it. This
data frame must has gene symbols as row names}

\item{GSP}{a vector containing Gold Standard Positive (GSP)}

\item{GSN}{a vector containing Gold Standard Negative (GSN)}

\item{method}{machine learning method. It can be one of "gbm" for
Gradient Boosting Machine (GBM), "svmRadial" for Support Vector
Machines with Radial Basis Function Kernel (SVM), "rda" for Regularized
Discriminant Analysis (RDA), "knn" for k-nearest neighbor (KNN), "pls"
for Partial Least Squares (PLS), "nnet" for Neural Network (NNET), "rf"
for Random Forest (RF), "myrf" for customised Random Forest (RF),
"cforest" for Conditional Inference Random Forest, "glmnet" for glmnet,
"glm" for Generalized Linear Model (GLM), "bayesglm" for Bayesian
Generalized Linear Model (BGLM), "LogitBoost" for Boosted Logistic
Regression (BLR), "xgbLinear" for eXtreme Gradient Boosting as linear
booster (XGBL), "xgbTree" for eXtreme Gradient Boosting as tree booster
(XGBT)}

\item{nfold}{an integer specifying the number of folds for cross
validataion. Per fold creates balanced splits of the data preserving
the overall distribution for each class (GSP and GSN), therefore
generating balanced cross-vallidation train sets and testing sets. By
default, it is 3 meaning 3-fold cross validation}

\item{nrepeat}{an integer specifying the number of repeats for cross
validataion. By default, it is 10 indicating the cross-validation
repeated 10 times}

\item{seed}{an integer specifying the seed}

\item{aggregateBy}{the aggregate method used to aggregate results from
repeated cross validataion. It can be either "none" for no aggregration
(meaning the best model based on all data used for cross validation is
used), or "orderStatistic" for the method based on the order statistics
of p-values, or "fishers" for Fisher's method, "Ztransform" for
Z-transform method, "logistic" for the logistic method. Without loss of
generality, the Z-transform method does well in problems where evidence
against the combined null is spread widely (equal footings) or when the
total evidence is weak; Fisher's method does best in problems where the
evidence is concentrated in a relatively small fraction of the
individual tests or when the evidence is at least moderately strong;
the logistic method provides a compromise between these two. Notably,
the aggregate methods 'Ztransform' and 'logistic' are preferred here}

\item{verbose}{logical to indicate whether the messages will be
displayed in the screen. By default, it sets to TRUE for display}

\item{RData.location}{the characters to tell the location of built-in
RData files. See \code{\link{xRDataLoader}} for details}

\item{guid}{a valid (5-character) Global Unique IDentifier for an OSF
project. See \code{\link{xRDataLoader}} for details}
}
\value{
an object of class "sTarget", a list with following components:
\itemize{
\item{\code{model}: an object of class "train" as a best model}
\item{\code{ls_model}: a list of best models from repeated
cross-validation}
\item{\code{priority}: a data frame of n X 5 containing gene priority
information, where n is the number of genes in the input data frame,
and the 5 columns are "GS" (either 'GSP', or 'GSN', or 'Putative'),
"name" (gene names), "rank" (priority rank), "rating" (5-star priority
score/rating), and "description" (gene description)}
\item{\code{predictor}: a data frame, which is the same as the input
data frame but inserting two additional columns ('GS' in the first
column, 'name' in the second column)}
\item{\code{performance}: a data frame of 1+nPredictor X 2 containing
the supervised/predictor performance info, where nPredictor is the
number of predictors, two columns are "ROC" (AUC values) and "Fmax"
(F-max values)}
\item{\code{performance_cv}: a data frame of nfold*nrepeat X 2
containing the repeated cross-validation performance, where two columns
are "ROC" (AUC values) and "Fmax" (F-max values)}
\item{\code{importance}: a data frame of nPredictor X 1 containing the
predictor importance info}
\item{\code{gp}: a ggplot object for the ROC curve}
\item{\code{gp_cv}: a ggplot object for the ROC curves from repeated
cross-validation}
\item{\code{evidence}: an object of the class "eTarget", a list with
following components "evidence" and "metag"}
\item{\code{list_pNode}: a list of "pNode" objects}
}
}
\description{
\code{xMLcaret} is supposed to integrate predictor matrix in a
supervised manner via machine learning algorithms using caret. The
caret package streamlines model building and performance evaluation. It
requires three inputs: 1) Gold Standard Positive (GSP) targets; 2) Gold
Standard Negative (GSN) targets; 3) a predictor matrix containing genes
in rows and predictors in columns, with their predictive scores inside
it. It returns an object of class 'sTarget'.
}
\note{
It will depend on whether a package "caret" and its suggested packages
have been installed. It can be installed via:
\code{BiocManager::install(c("caret","e1071","gbm","kernlab","klaR","pls","nnet","randomForest","party","glmnet","arm","caTools","xgboost"))}.
}
\examples{
RData.location <- "http://galahad.well.ox.ac.uk/bigdata"
\dontrun{
sTarget <- xMLcaret(df_prediction, GSP, GSN, method="myrf")
}
}
\seealso{
\code{\link{xPierMatrix}}, \code{\link{xPredictROCR}},
\code{\link{xPredictCompare}}, \code{\link{xSparseMatrix}},
\code{\link{xSymbol2GeneID}}
}
