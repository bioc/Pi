#' Function to visualise prioritised genes in terms of a KEGG pathway
#'
#' \code{xPierKEGG} is supposed to visualise prioritised genes in terms of a KEGG pathway. It returns an object of class "igraph". 
#'
#' @param xTarget an object of class "dTarget" or "sTarget"
#' @param vis the type of visualisation for a KEGG pathway. It can be one of "net" (visualising the network with nodes colored by priority score; by default), "evidence" for visualising the network with nodes as pie charts, and "pathview" for using the package "pathview"
#' @param hsa the identity of KEGG pathway in query. The full list of pathways in human can be found at \url{http://www.genome.jp/kegg-bin/show_organism?menu_type=pathway_maps&org=hsa}. For example, 'hsa04621' for 'NOD-like receptor signaling pathway', where the prefix 'hsa' can be ignored
#' @param priority.top the number of the top targets. By default, it is NULL meaning no such restriction
#' @param incoming.neighbor.order an integer giving the order of the incoming neighborhood. By default, it is 1-order incoming neighborhood
#' @param nodes_query which gene in query will be visualised. It (if not null) has the high priority over nodes selected by 'priority.top' and 'incoming.neighbor.order' above
#' @param largest.comp logical to indicate whether the largest component is only retained. By default, it sets to true for the largest component being left
#' @param pathview.filename the file name saved using the package "pathview". By default, it is NULL meaning "hsa.Pi"
#' @param pathview.filetype the file format saved using the package "pathview". It can be "png" or "pdf"
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to true
#' @param RData.location the characters to tell the location of built-in RData files. See \code{\link{xRDataLoader}} for details
#' @param ... additional graphic parameters. If the type of visualisation is 'net', see \code{\link{xVisNet}}; if the visualisation type is 'evidence', see \code{\link{xVisEvidence}}
#' @return
#' a subgraph, an object of class "igraph".
#' @note If vis is 'pathview', it will depend on whether a package "pathview" has been installed. It can be installed via: \code{source("http://bioconductor.org/biocLite.R"); biocLite("pathview")}.
#' @export
#' @seealso \code{\link{xVisNet}}, \code{\link{xVisEvidence}}
#' @include xPierKEGG.r
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' ## evidence
#' xPierKEGG(xTarget, hsa="hsa04621", vis="evidence", RData.location=RData.location)
#' ## network
#' xPierKEGG(xTarget, hsa="hsa04621", vis="net", RData.location=RData.location)
#' ## using pathview
#' pv.out <- xPierKEGG(xTarget, hsa="hsa04621", vis="pathview", pathview.filetype=c("png","pdf")[2], RData.location=RData.location)
#' }

xPierKEGG <- function(xTarget, vis=c("net","evidence","pathview"), hsa="hsa04621", priority.top=NULL, incoming.neighbor.order=1, nodes_query=NULL, largest.comp=TRUE, pathview.filename=NULL, pathview.filetype=c("png","pdf"), verbose=TRUE, RData.location="http://galahad.well.ox.ac.uk/bigdata", ...)
{

    vis <- match.arg(vis)
    pathview.filetype <- match.arg(pathview.filetype)

    if(class(xTarget) == "dTarget"){
        df_priority <- xTarget$priority[, c("rank","priority")]
    }else if(class(xTarget) == "sTarget"){
        df_priority <- xTarget$priority[, c("rank","priority")]
    }else{
    	stop("The function must apply to a 'dTarget' or 'sTarget' or 'eTarget' object.\n")
    }
	
	ls_ig <- xRDataLoader(RData.customised="ig.KEGG.list", RData.location=RData.location)
	kegg <- sapply(ls_ig, function(x) x$path)
	kegg <- gsub('^path:', '', kegg)
	hsa <- gsub('^path:', '', hsa)
	ind <- match(hsa, kegg)
	if(is.na(ind)){
		ind <- grep(hsa, names(kegg))
		if(length(ind)==0){
			warning(sprintf("No found for queried '%s'", hsa), appendLF=TRUE)
			return(NULL)
		}else if(length(ind)>1){
			warning(sprintf("%d found for queried %s: only 1st kept", length(ind), hsa), appendLF=TRUE)
			ind <- ind[1]
		}
	}
	if(verbose){
		now <- Sys.time()
		message(sprintf("Visualising '%s: %s' (%s) ...", kegg[ind], names(kegg[ind]), as.character(now)), appendLF=TRUE)
	}
	ig <- ls_ig[[ind]]
	
	if(vis=="pathview"){
		gene.data <- df_priority$priority
		names(gene.data) <- rownames(df_priority)
		pathway.id <- kegg[ind]
		
		if(pathview.filetype=="png"){
			kegg.native <- TRUE
		}else if(pathview.filetype=="pdf"){
			kegg.native <- FALSE
		}
		out.suffix <- "pv"
		
		#pv.out <- pathview::pathview(gene.data=gene.data, pathway.id=pathway.id, gene.idtype="symbol", species="hsa", kegg.native=FALSE, out.suffix="Pi", discrete=list(gene=FALSE), limit=list(gene=5), bins=list(gene=10), both.dirs=list(gene=FALSE), trans.fun=list(gene=NULL), low=list(gene="white"), mid=list(gene="yellow"), high=list(gene="red"), na.col="transparent", new.signature=FALSE, afactor=4, pdf.size=c(7,7), res=300, cex=0.4)
		#pv.out <- pathview::pathview(gene.data=gene.data, pathway.id=pathway.id, gene.idtype="symbol", species="hsa", kegg.native=kegg.native, ...)
		
		pv.out <- pathview::pathview(gene.data=gene.data, pathway.id=pathway.id, gene.idtype="symbol", species="hsa", kegg.native=kegg.native, out.suffix=out.suffix, discrete=list(gene=FALSE), limit=list(gene=5), bins=list(gene=10), both.dirs=list(gene=FALSE), trans.fun=list(gene=NULL), low=list(gene="white"), mid=list(gene="yellow"), high=list(gene="red"), na.col="transparent", new.signature=FALSE, afactor=4, pdf.size=c(7,7), res=300, cex=0.4, ...)
		
		if(is.null(pathview.filename)){
			pathview.filename <- paste0(pathway.id, ".Pi")
		}
		outputfile <- paste0(pathview.filename, ".", pathview.filetype)
		
		tfile <- paste0(pathway.id, ".", out.suffix, ".", pathview.filetype)
		if(file.exists(tfile)){
			file.copy(from=tfile, to=outputfile, overwrite=TRUE, recursive=FALSE, copy.mode=TRUE)
            message(sprintf("Congratulations! A file '%s' (in the directory %s) has been created!", outputfile, getwd()), appendLF=TRUE)
			
			unlink(tfile, recursive=TRUE, force=TRUE)
			unlink(paste0(pathway.id,'.xml'), recursive=TRUE, force=TRUE)
			unlink(paste0(pathway.id,'.png'), recursive=TRUE, force=TRUE)
		}
		
		return(pv.out)
	}else{
	
		## if given, it has the higher priority
		if(!is.null(nodes_query)){
			ind <- match(V(ig)$name, nodes_query)
			nodes_query <- V(ig)$name[!is.na(ind)]
		}
	
		if(length(nodes_query)==0){
			##############
			## priority top
			if(!is.null(priority.top)){
				priority.top <- as.integer(priority.top)
				if(priority.top > nrow(df_priority)){
					priority.top <- nrow(df_priority)
				}else if(priority.top <= 1){
					priority.top <- nrow(df_priority)
				}
			}else{
				priority.top <- nrow(df_priority)
			}
			df_priority_top <- df_priority[1:priority.top,]
			###############
			ind <- match(V(ig)$name, rownames(df_priority_top))
			nodes_query <- V(ig)$name[!is.na(ind)]
			## including incoming neighbors
			neighs.out <- igraph::neighborhood(ig, order=incoming.neighbor.order, nodes=nodes_query, mode="in")
			neighbors <- unique(names(unlist(neighs.out)))
			ind <- match(neighbors, rownames(df_priority))
			neighbors <- neighbors[!is.na(ind)]
			subg <- dnet::dNetInduce(ig, nodes_query=neighbors, knn=0, remove.loops=TRUE, largest.comp=largest.comp)
		
			if(verbose){
				message(sprintf("Out of %d genes, %d in the top %d, %d (inclusion of %d-order incoming neighbors), and %d (within the largest connected network) are visualised", vcount(ig), length(nodes_query), priority.top, length(neighbors), incoming.neighbor.order, vcount(subg)), appendLF=TRUE)
			}
		
		}else{
			subg <- dnet::dNetInduce(ig, nodes_query=nodes_query, knn=0, remove.loops=TRUE, largest.comp=largest.comp)
			if(verbose){
				message(sprintf("Out of %d genes, %d in query and %d (within the network) are visualised", vcount(ig), length(nodes_query), vcount(subg)), appendLF=TRUE)
			}
		}
		
		ind <- match(V(subg)$name, rownames(df_priority))
		V(subg)$rank <- as.numeric(df_priority$rank)[ind]
		V(subg)$priority <- as.numeric(df_priority$priority)[ind]
		
		if(vis=="net"){
			ind <- match(V(subg)$name, rownames(df_priority))
			pattern <- as.numeric(df_priority$priority)[ind[!is.na(ind)]]
			names(pattern) <- rownames(df_priority)[ind[!is.na(ind)]]
			xVisNet(subg, pattern=pattern, ...)
		}else if(vis=="evidence"){
			xVisEvidence(xTarget=xTarget, g=subg, nodes=V(subg)$name, mtext.side=NA, ...)
		}
	
		return(subg)
	}
}
