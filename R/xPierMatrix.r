#' Function to extract priority matrix from a list of pNode objects
#'
#' \code{xPierMatrix} is supposed to extract priority matrix from a list of pNode objects 
#'
#' @param list_pNode a list of "pNode" objects
#' @param displayBy which priority will be extracted. It can be "score" for priority score (by default), "rank" for priority rank
#' @param combineBy how to resolve nodes/targets from a list of "pNode" objects. It can be "intersect" for intersecting nodes (by default), "union" for unionising nodes
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to true for display
#' @return
#' a data frame containing priority matrix.
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

xPierMatrix <- function(list_pNode, displayBy=c("score","rank"), combineBy=c('intersect','union'), verbose=TRUE)
{

    displayBy <- match.arg(displayBy)
    combineBy <- match.arg(combineBy)
    
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
		ind <- ind[!is.na(ind)]
		if(displayBy=='score'){
			res <- p[ind, c("priority")]
		}else if(displayBy=='rank'){
			res <- p[ind, c("rank")]
		}
	})
	df_priority <- do.call(cbind, ls_priority)
	rownames(df_priority) <- nodes
	
    invisible(df_priority)
}
