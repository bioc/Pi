######################################################################
# pNode
######################################################################
#' @title Definition for S3 class \code{pNode}
#' @description \code{pNode} has 7 components: priority, g, SNP, Gene2SNP, nGenes, eGenes and cGenes.
#' @param priority a data frame
#' @param g an 'igraph' object
#' @param SNP a data frame
#' @param Gene2SNP a data frame
#' @param nGenes a data frame
#' @param eGenes a data frame
#' @param cGenes a data frame
#' @return an object of S3 class \code{pNode}
#' @keywords S3 classes
#' @export
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' pNode(evidence, metag)
#' }
pNode <- function(priority, g, SNP, Gene2SNP, nGenes, eGenes, cGenes){
	## integrity checks
	if(class(priority)!='data.frame' | class(g)!='igraph'){
		stop("The S3 class 'pNode' object failed to pass integrity checks!\n")
	}
	value <- list(priority=priority, g=g, SNP=SNP, Gene2SNP=Gene2SNP, nGenes=nGenes, eGenes=eGenes, cGenes=cGenes)
	class(value) <- "pNode"
	return(value)
}
#' @param x an object of class \code{pNode}
#' @param ... other parameters
#' @rdname pNode
#' @export
print.pNode <- function(x, ...) {
	cat(sprintf("An object of S3 class '%s', with %d components:", class(x), length(names(x))), "\n", sep="")
	cat(sprintf("  $g: an igraph object with %d nodes and %d edges", vcount(x$g), ecount(x$g)), "\n", sep="")
	cat(sprintf("  $priority: a data frame of %d rows X %d columns", dim(x$priority)[1],dim(x$priority)[2]), "\n", sep="")
	if(!is.null(x$SNP)){
		cat(sprintf("  $SNP: a data frame of %d rows X %d columns", dim(x$SNP)[1],dim(x$SNP)[2]), "\n", sep="")
	}
	if(!is.null(x$Gene2SNP)){
		cat(sprintf("  $Gene2SNP: a data frame of %d rows X %d columns", dim(x$Gene2SNP)[1],dim(x$Gene2SNP)[2]), "\n", sep="")
	}
	if(!is.null(x$nGenes)){
		cat(sprintf("  $nGenes: a data frame of %d rows X %d columns", dim(x$nGenes)[1],dim(x$nGenes)[2]), "\n", sep="")
	}
	if(!is.null(x$eGenes)){
		cat(sprintf("  $eGenes: a data frame of %d rows X %d columns", dim(x$eGenes)[1],dim(x$eGenes)[2]), "\n", sep="")
	}
	if(!is.null(x$cGenes)){
		cat(sprintf("  $cGenes: a data frame of %d rows X %d columns", dim(x$cGenes)[1],dim(x$cGenes)[2]), "\n", sep="")
	}
	cat("\n--------------------------------------------------\n")
	cat("$priority:\n")
	print(x$priority[1:2,], row.names=FALSE)
	cat("......\n")
	if(!is.null(x$SNP)){
		cat("$SNP:\n")
		print(x$SNP[1:2,], row.names=FALSE)
		cat("......\n")
	}
	if(!is.null(x$Gene2SNP)){
		cat("$Gene2SNP:\n")
		print(x$Gene2SNP[1:2,], row.names=FALSE)
		cat("......\n")
	}
	if(!is.null(x$nGenes)){
		cat("$nGenes:\n")
		print(x$nGenes[1:2,], row.names=FALSE)
		cat("......\n")
	}
	if(!is.null(x$eGenes)){
		cat("$eGenes:\n")
		print(x$eGenes[1:2,], row.names=FALSE)
		cat("......\n")
	}
	if(!is.null(x$cGenes)){
		cat("$cGenes:\n")
		print(x$cGenes[1:2,], row.names=FALSE)
		cat("......\n")
	}
}

######################################################################
# eTarget
######################################################################
#' @title Definition for S3 class \code{eTarget}
#' @description \code{eTarget} has 2 components: evidence and metag.
#' @param evidence a data frame
#' @param metag an 'igraph' object
#' @return an object of S3 class \code{eTarget}
#' @keywords S3 classes
#' @export
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' eTarget(evidence, metag)
#' }
eTarget <- function(evidence, metag){
	## integrity checks
	if(class(evidence)!='data.frame' | class(metag)!='igraph'){
		stop("The S3 class 'eTarget' object failed to pass integrity checks!\n")
	}
	value <- list(evidence=evidence, metag=metag)
	class(value) <- "eTarget"
	return(value)
}
#' @param x an object of class \code{eTarget}
#' @param ... other parameters
#' @rdname eTarget
#' @export
print.eTarget <- function(x, ...) {
	cat(sprintf("An object of S3 class '%s', with %d components:", class(x), length(names(x))), "\n", sep="")
	cat(sprintf("  $metag: an igraph object with %d nodes and %d edges", vcount(x$metag), ecount(x$metag)), "\n", sep="")
	cat(sprintf("  $evidence: a data frame of %d rows X %d columns", dim(x$evidence)[1],dim(x$evidence)[2]), "\n", sep="")
	cat("\n--------------------------------------------------\n")
	cat("$evidence:\n")
	print(x$evidence[1:2,], row.names=FALSE)
	cat("......\n")
}

######################################################################
# dTarget
######################################################################
#' @title Definition for S3 class \code{dTarget}
#' @description \code{dTarget} has 3 components: priority, predictor and metag.
#' @param priority a data frame
#' @param predictor a data frame
#' @param metag an 'igraph' object
#' @return an object of S3 class \code{dTarget}
#' @keywords S3 classes
#' @export
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' dTarget(priority, predictor, metag)
#' }
dTarget <- function(priority, predictor, metag){
	## integrity checks
	if(class(priority)!='data.frame' | class(predictor)!='data.frame' | class(metag)!='igraph'){
		stop("The S3 class 'dTarget' object failed to pass integrity checks!\n")
	}
	value <- list(priority=priority, predictor=predictor, metag=metag)
	class(value) <- "dTarget"
	return(value)
}
#' @param x an object of class \code{dTarget}
#' @param ... other parameters
#' @rdname dTarget
#' @export
print.dTarget <- function(x, ...) {
	cat(sprintf("An object of S3 class '%s', with %d components:", class(x), length(names(x))), "\n", sep="")
	cat(sprintf("  $metag: an igraph object with %d nodes and %d edges", vcount(x$metag), ecount(x$metag)), "\n", sep="")
	cat(sprintf("  $predictor: a data frame of %d rows X %d columns", dim(x$predictor)[1],dim(x$predictor)[2]), "\n", sep="")
	cat(sprintf("  $priority: a data frame of %d rows X %d columns", dim(x$priority)[1],dim(x$priority)[2]), "\n", sep="")
	cat("\n--------------------------------------------------\n")
	cat("$priority:\n")
	print(x$priority[1:2,], row.names=FALSE)
	cat("......\n")
}

######################################################################
# sTarget
######################################################################
#' @title Definition for S3 class \code{sTarget}
#' @description \code{sTarget} mush have following components: priority, predictor, performance, importance, evidence.
#' @param priority a data frame
#' @param predictor a data frame
#' @param performance a data frame
#' @param importance a data frame
#' @param evidence an 'eTarget' object
#' @return an object of S3 class \code{sTarget}
#' @keywords S3 classes
#' @export
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' sTarget(priority, predictor, performance, importance, evidence)
#' }
sTarget <- function(priority, predictor, performance, importance, evidence){
	## integrity checks
	if(class(priority)!='data.frame' | class(evidence)!='eTarget'){
		stop("The S3 class 'sTarget' object failed to pass integrity checks!\n")
	}
	value <- list(priority=priority, predictor=predictor, performance=performance, importance=importance, evidence=evidence)
	class(value) <- "sTarget"
	return(value)
}
#' @param x an object of class \code{sTarget}
#' @param ... other parameters
#' @rdname sTarget
#' @export
print.sTarget <- function(x, ...) {
	cat(sprintf("An object of S3 class '%s', with %d components including:", class(x), length(names(x))), "\n", sep="")
	cat(sprintf("  $priority: a data frame of %d rows X %d columns", dim(x$priority)[1],dim(x$priority)[2]), "\n", sep="")
	cat(sprintf("  $predictor: a data frame of %d rows X %d columns", dim(x$predictor)[1],dim(x$predictor)[2]), "\n", sep="")
	cat(sprintf("  $performance: a data frame of %d rows X %d columns", dim(x$performance)[1],dim(x$performance)[2]), "\n", sep="")
	cat(sprintf("  $importance: a data frame of %d rows X %d columns", dim(x$importance)[1],dim(x$importance)[2]), "\n", sep="")
	cat(sprintf("  $evidence: an object of the class 'eTarget'"), "\n", sep="")
	cat("\n--------------------------------------------------\n")
	cat("$priority:\n")
	print(x$priority[1:2,], row.names=FALSE)
	cat("......\n")
	cat("$performance:\n")
	print(x$performance[1:2,], row.names=FALSE)
	cat("......\n")
	cat("$importance:\n")
	print(x$importance[1:2,], row.names=FALSE)
	cat("......\n")
	cat("$evidence:\n")
	print(x$evidence)
}

######################################################################
# cTarget
######################################################################
#' @title Definition for S3 class \code{cTarget}
#' @description \code{cTarget} has 2 components: priority and predictor.
#' @param priority a data frame
#' @param predictor a data frame
#' @return an object of S3 class \code{cTarget}
#' @keywords S3 classes
#' @export
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' cTarget(priority, predictor)
#' }
cTarget <- function(priority, predictor){
	## integrity checks
	if(class(priority)!='data.frame' | class(predictor)!='data.frame'){
		stop("The S3 class 'cTarget' object failed to pass integrity checks!\n")
	}
	value <- list(priority=priority, predictor=predictor)
	class(value) <- "cTarget"
	return(value)
}
#' @param x an object of class \code{cTarget}
#' @param ... other parameters
#' @rdname cTarget
#' @export
print.cTarget <- function(x, ...) {
	cat(sprintf("An object of S3 class '%s', with %d components:", class(x), length(names(x))), "\n", sep="")
	cat(sprintf("  $predictor: a data frame of %d rows X %d columns", dim(x$predictor)[1],dim(x$predictor)[2]), "\n", sep="")
	cat(sprintf("  $priority: a data frame of %d rows X %d columns", dim(x$priority)[1],dim(x$priority)[2]), "\n", sep="")
	cat("\n--------------------------------------------------\n")
	cat("$priority:\n")
	print(x$priority[1:2,], row.names=FALSE)
	cat("......\n")
	cat("$predictor:\n")
	print(x$predictor[1:2,], row.names=FALSE)
	cat("......\n")
}



