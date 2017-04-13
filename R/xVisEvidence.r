#' Function to visualise evidence for prioritised genes in a gene network
#'
#' \code{xVisEvidence} is supposed to visualise evidence for prioritised genes in a gene network. It returns an object of class "igraph". 
#'
#' @param xTarget an object of class "dTarget", "sTarget" or "eTarget"
#' @param g an object of class "igraph". If NA, the 'metag' will be used, which is part of the input object "xTarget"
#' @param nodes which node genes are in query. If NULL, the top gene will be queried
#' @param node.info tells the additional information used to label nodes. It can be one of "none" (only gene labeling), "smart" for (by default) using three pieces of information (if any): genes, 5-star ratings, and associated ranks (marked by an @ icon)
#' @param neighbor.order an integer giving the order of the neighborhood. By default, it is 1-order neighborhood
#' @param neighbor.seed logical to indicate whether neighbors are seeds only. By default, it sets to true
#' @param neighbor.top the top number of the neighbors with the highest priority. By default, it sets to NULL to disable this parameter
#' @param colormap short name for the colormap. It can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta), and "ggplot2" (emulating ggplot2 default color palette). Alternatively, any hyphen-separated HTML color names, e.g. "lightyellow-orange" (by default), "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}
#' @param legend.position the legend position. If NA, the legend will be hiden
#' @param legend.horiz logical specifying the legend horizon. If TRUE, set the legend horizontally rather than vertically
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to true for display
#' @param ... additional graphic parameters. See \url{http://igraph.org/r/doc/plot.common.html} for the complete list.
#' @return
#' a subgraph, an object of class "igraph".
#' @export
#' @seealso \code{\link{xPierMatrix}}
#' @include xVisEvidence.r
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' ## TNFRSF1A
#' xVisEvidence(xTarget, nodes="TNFRSF1A", neighbor.order=1, neighbor.seed=TRUE, neighbor.top=NULL, vertex.label.color="black", vertex.label.cex=0.7, vertex.label.dist=0.6, vertex.label.font=4, legend.position="bottomleft", legend.horiz=TRUE, newpage=FALSE)
#' ## UBA52
#' xVisEvidence(xTarget, nodes="UBA52", neighbor.order=1, neighbor.seed=TRUE, neighbor.top=20, vertex.label.color="black", vertex.label.cex=0.7, vertex.label.dist=0.6, vertex.label.font=4, legend.position="bottomleft", legend.horiz=TRUE, newpage=FALSE)
#' }

xVisEvidence <- function(xTarget, g=NA, nodes=NULL, node.info=c("smart","none"), neighbor.order=1, neighbor.seed=TRUE, neighbor.top=NULL, colormap="ggplot2", legend.position="topleft", legend.horiz=FALSE, verbose=TRUE, ...)
{

    node.info <- match.arg(node.info)

    if(class(xTarget) == "dTarget"){
        df_evidence <- xTarget$priority[, 7:ncol(xTarget$priority)]
        df_priority <- xTarget$priority[, c("rank","priority")]
		
    }else if(class(xTarget) == "sTarget"){
        df_evidence <- xTarget$evidence$evidence
        df_priority <- xTarget$priority[, c("rank","priority")]
		
    }else if(class(xTarget) == "eTarget"){
        df_evidence <- xTarget$evidence
        # here, sorted by the number of seed gene types
        df_priority <- df_evidence[order(df_evidence[,1],decreasing=TRUE),]
		
		#neighbor.top <- NULL
		
    }else{
    	stop("The function must apply to a 'dTarget' or 'sTarget' or 'eTarget' object.\n")
    }
	
	
	if(class(g) == "igraph"){
		g <- g
	}else{
		if(class(xTarget) == "dTarget" | class(xTarget) == "eTarget"){
			g <- xTarget$metag
		}else if(class(xTarget) == "sTarget"){
			g <- xTarget$evidence$metag
		}
	}
	if(class(g)!='igraph'){
		stop("The input 'g' must be provided!\n")
	}
	
	if(is.null(nodes)){
		nodes <- rownames(df_evidence)[1]
	}else{
		ind <- match(nodes, rownames(df_evidence))
		ind <- which(!is.na(ind))
		if(length(ind)>=1){
			nodes <- nodes[ind]
		}else{
			nodes <- rownames(df_evidence)[1]
		}
	}
    
    neighs.out <- igraph::neighborhood(g, order=neighbor.order, nodes=nodes, mode="all")
	neighbors <- names(unlist(neighs.out))
	if(neighbor.seed){
		# restrict to seeds
		ind <- neighbors %in% rownames(df_evidence)[df_evidence[,1]>0]
		neighbors <- neighbors[ind]
	}
	if(!is.null(neighbor.top)){
		neighbor.top <- as.integer(neighbor.top)
		if(neighbor.top > length(neighbors)){
			neighbor.top <- length(neighbors)
		}
		
		ind <- match(rownames(df_priority), neighbors)
		df_neighbors <- df_priority[!is.na(ind),]
		neighbors <- rownames(df_neighbors)[1:neighbor.top]
	}
	vids <- union(neighbors, nodes)
    subg <- dnet::dNetInduce(g, nodes_query=vids, knn=0, remove.loops=TRUE, largest.comp=TRUE)
    
	if(verbose){
		now <- Sys.time()
		message(sprintf("The %d-order graph induced by %d input gene(s): %d nodes and %d edges", neighbor.order, length(nodes), vcount(subg), ecount(subg)), appendLF=TRUE)
	}
	
	ind <- match(V(subg)$name, rownames(df_evidence))
	df_val <- df_evidence[ind,]
	ls_val <- lapply(1:nrow(df_val), function(i){
		if(sum(df_val[i,-1])==0){
			NULL
		}else{
			as.numeric(df_val[i,-1]!=0)
		}
	})
	names(ls_val) <- rownames(df_val)
	
	## vertex.label
	vertex.label <- V(subg)$name
	if(class(xTarget) != "eTarget"){
		ind <- match(vertex.label, rownames(df_priority))
		df_nodes <- df_priority[ind, ]
		if(node.info=='smart'){
			vertex.label <- paste0(vertex.label, '\n[', signif(df_nodes$priority,digits=2), '@', df_nodes$rank, ']')
		}
	}
	
	## nodes NULL are drawn as circles
	vertex.shape <- rep("pie", length(ls_val))
	vertex.shape[sapply(ls_val, is.null)] <- "circle"
	## pie color
	pie.color <- xColormap(colormap)(ncol(df_val)-1)
	## legend text
	legend.text <- colnames(df_val)[-1]
	legend.text[grep('OMIM|disease',legend.text,ignore.case=TRUE)] <- "dGene"
	legend.text[grep('Phenotype',legend.text,ignore.case=TRUE)] <- "pGene"
	legend.text[grep('Function',legend.text,ignore.case=TRUE)] <- "fGene"
	legend.text[grep('nearbyGenes',legend.text,ignore.case=TRUE)] <- "nGene"
	legend.text[grep('eQTL',legend.text,ignore.case=TRUE)] <- "eGene"
	legend.text[grep('HiC|Hi-C',legend.text,ignore.case=TRUE)] <- "hGene"
	## vertex size
	vertex.size <- igraph::degree(subg)
	if(min(vertex.size) == max(vertex.size)){
		vertex.size <- 12
	}else{
		vertex.size <- 12 * (vertex.size - min(vertex.size))/(max(vertex.size) - min(vertex.size)) + 8
	}
	## draw graph
	xVisNet(subg, vertex.shape=vertex.shape, vertex.pie=ls_val, vertex.pie.color=list(pie.color), vertex.pie.border="grey", vertex.label=vertex.label, vertex.color="grey", vertex.size=vertex.size, signature=FALSE, ...)
	if(!is.na(legend.position)){
		legend(legend.position, legend=legend.text, col=pie.color, pch=10, bty="n", pt.cex=1.2, cex=1, text.col="darkgrey", text.font=4, horiz=legend.horiz)
	}
	
    return(subg)
}
