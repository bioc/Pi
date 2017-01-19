#' Function to extract priority matrix from a list of pNode objects
#'
#' \code{xPierMatrix} is supposed to extract priority matrix from a list of pNode objects 
#'
#' @param list_pNode a list of "pNode" objects
#' @param displayBy which priority will be extracted. It can be "score" for priority score (by default), "rank" for priority rank, "pvalue" for priority p-value
#' @param combineBy how to resolve nodes/targets from a list of "pNode" objects. It can be "intersect" for intersecting nodes (by default), "union" for unionising nodes
#' @param aggregateBy the aggregate method used. It can be either "none" for no aggregation, or "orderStatistic" for the method based on the order statistics of p-values, "fishers" for Fisher's method, "Ztransform" for Z-transform method
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to true for display
#' @return
#' If aggregateBy is 'none' (by default), a data frame containing priority matrix, with each column for either priority score, or priorty rank or priority p-value.
#' If aggregateBy is not 'none', an object of the class "aTarget", a list with following components:
#' \itemize{
#'  \item{\code{priority}: a data frame of nGene X 5 containing gene priority (aggregated) information, where nGene is the number of genes, and the 5 columns are "name" (gene names), "rank" (ranks of the priority scores), "pvalue" (the aggregated p-value, converted from empirical cumulative distribution of the probability of being GSP), "fdr" (fdr adjusted from the aggregated p-value), "priority" (-log10(fdr))}}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note none
#' @export
#' @seealso \code{\link{xPierSNPs}}
#' @include xPierMatrix.r
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' df_score <- xPierMatrix(ls_pNode)
#' }

xPierMatrix <- function(list_pNode, displayBy=c("score","rank","pvalue"), combineBy=c('intersect','union'), aggregateBy=c("none","orderStatistic","fishers","Ztransform"), verbose=TRUE)
{

    displayBy <- match.arg(displayBy)
    combineBy <- match.arg(combineBy)
    aggregateBy <- match.arg(aggregateBy) 
    
   	if(any(class(list_pNode) %in% c("pNode"))){
		list_pNode <- list(list_pNode)
	}else if(class(list_pNode)=="list"){
		list_pNode <- list_pNode
	}else{
		stop("The function must apply to an 'list' object or an 'pNode' object.\n")
	}
	
	## get nodes involved
	ls_nodes <- lapply(list_pNode, function(x){
		V(x$g)$name
	})
	if(combineBy=='intersect'){
		nodes <- base::Reduce(intersect, ls_nodes)
	}else if(combineBy=='union'){
		nodes <- base::Reduce(union, ls_nodes)
	}
	nodes <- sort(nodes)
	
	## Combine into a data frame called 'df_priority'
	list_names <- names(list_pNode)
	if(is.null(list_names)){
		list_names <- paste('Predictor', 1:length(list_pNode), sep=' ')
	}
	ls_priority <- lapply(list_pNode, function(pNode){
		p <- pNode$priority
		ind <- match(nodes, rownames(p))
		#ind <- ind[!is.na(ind)]
		if(displayBy=='score' | displayBy=='pvalue'){
			res <- p[ind, c("priority")]
		}else if(displayBy=='rank'){
			res <- p[ind, c("rank")]
		}
	})
	df_priority <- do.call(cbind, ls_priority)
	rownames(df_priority) <- nodes
	
	## replace NA with worst value
	if(displayBy=='score' | displayBy=='pvalue'){
		df_priority[is.na(df_priority)] <- 0
	}else if(displayBy=='rank'){
		df_priority[is.na(df_priority)] <- length(nodes)
	}
	
	## only when displayBy=='pvalue'
	## Convert into p-values by computing an empirical cumulative distribution function
	if(displayBy=='pvalue'){
		df_full <- df_priority
		ls_pval <- lapply(1:ncol(df_full), function(j){
			x <- df_full[,j]
			my.CDF <- stats::ecdf(x)
			pval <- 1 - my.CDF(x)
		})
		df_pval <- do.call(cbind, ls_pval)
		rownames(df_pval) <- rownames(df_full)
		colnames(df_pval) <- colnames(df_full)
		
		df_priority <- df_pval
		## aggregate p values
		if(aggregateBy != "none"){
			df_ap <- dnet::dPvalAggregate(pmatrix=df_priority, method=aggregateBy)
			
			df_ap <- sort(df_ap, decreasing=FALSE)
			## get rank
			df_rank <- rank(df_ap, ties.method="min")
			######
			df_ap[df_ap==0] <- min(df_ap[df_ap!=0])
			######
			## adjp
			df_adjp <- stats::p.adjust(df_ap, method="BH")
			
			df_priority <- data.frame(name=names(df_ap), rank=df_rank, pvalue=df_ap, fdr=df_adjp, priority=-log10(df_adjp), stringsAsFactors=FALSE)
			
			aTarget <- list(priority = df_priority,
							Call     = match.call()
						 )
			class(aTarget) <- "aTarget"
			
			df_priority <- aTarget
		}
		
	}
	
	if(verbose){
		
		if(displayBy=="pvalue" & aggregateBy!="none"){
			message(sprintf("A total of %d genes are prioritised, combined by '%s' and aggregated by '%s' from %d predictors", nrow(df_priority$priority), combineBy, aggregateBy, length(list_pNode)), appendLF=TRUE)
		}else{
			message(sprintf("A matrix of %d genes x %d predictors are generated, displayed by '%s' and combined by '%s'", nrow(df_priority), ncol(df_priority), displayBy, combineBy), appendLF=TRUE)
		}
		
	}
	
	
    invisible(df_priority)
}
