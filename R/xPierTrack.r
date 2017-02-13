#' Function to visualise prioritised genes using track plot
#'
#' \code{xPierTrack} is supposed to visualise prioritised genes using track plot. Priority for the gene in query is displayed on the data track and nearby genes on the annotation track. Genomic locations on the X-axis are indicated on the X-axis, and the gene in query is highlighted.
#'
#' @param pNode an object of class "pNode" (or "pTarget" or "dTarget")
#' @param priority.top the number of the top targets used for track plot. By default, it is NULL meaning all targets are used
#' @param target.query which gene in query will be visualised. If NULL, the target gene with the top priority will be displayed
#' @param window the maximum distance defining nearby genes around the target gene in query. By default it is 1e6
#' @param nearby the maximum number defining nearby genes around the target gene in query. By default it is NULL. If not NULL, it will overwrite the parameter 'window'
#' @param query.highlight logical to indicate whether the gene in query will be highlighted
#' @param GR.Gene the genomic regions of genes. By default, it is 'UCSC_knownGene', that is, UCSC known genes (together with genomic locations) based on human genome assembly hg19. It can be 'UCSC_knownCanonical', that is, UCSC known canonical genes (together with genomic locations) based on human genome assembly hg19. Alternatively, the user can specify the customised input. To do so, first save your RData file (containing an GR object) into your local computer, and make sure the GR object content names refer to Gene Symbols. Then, tell "GR.Gene" with your RData file name (with or without extension), plus specify your file RData path in "RData.location"
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to false for no display
#' @param RData.location the characters to tell the location of built-in RData files. See \code{\link{xRDataLoader}} for details
#' @return an object of class "ggplot", appended by an GR object called 'gr'
#' @note none
#' @export
#' @seealso \code{\link{xMLrandomforest}}
#' @include xPierTrack.r
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#'
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' # a) provide the SNPs with the significance info
#' ## get lead SNPs reported in AS GWAS and their significance info (p-values)
#' #data.file <- "http://galahad.well.ox.ac.uk/bigdata/AS.txt"
#' #AS <- read.delim(data.file, header=TRUE, stringsAsFactors=FALSE)
#' ImmunoBase <- xRDataLoader(RData.customised='ImmunoBase', RData.location=RData.location)
#' gr <- ImmunoBase$AS$variants
#' AS <- as.data.frame(GenomicRanges::mcols(gr)[, c('Variant','Pvalue')])
#'
#' \dontrun{
#' # b) perform priority analysis
#' pNode <- xPierSNPs(data=AS, include.eQTL="JKng_mono", include.HiC='Monocytes', network="PCommonsUN_medium", restart=0.7, RData.location=RData.location)
#'
#' # c) track plot
#' library(Gviz)
#' #pdf(file="Gene_tracks.pdf", height=4, width=10, compress=TRUE)
#' xPierTrack(pNode, RData.location=RData.location)
#' #dev.off()
#' xPierTrack(pNode, priority.top=1000, nearby=20, RData.location=RData.location)
#' }

xPierTrack <- function(pNode, priority.top=NULL, target.query=NULL, window=1e6, nearby=NULL, query.highlight=TRUE, GR.Gene=c("UCSC_knownGene","UCSC_knownCanonical"), verbose=TRUE, RData.location="http://galahad.well.ox.ac.uk/bigdata")
{

    if(class(pNode) == "pNode"){
        df_priority <- pNode$priority[, c("seed","weight","priority")]
    }else if(class(pNode) == "pTarget"){
    	df_priority <- pNode$priority[, c("pvalue","fdr","priority")]
    }else if(class(pNode) == "dTarget"){
    	df_priority <- pNode$priority[, c("pvalue","fdr","priority")]	
    }else{
    	stop("The function must apply to a 'pNode' or 'pTarget' or 'dTarget' object.\n")
    }
    
   ###############
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
    df_priority <- df_priority[1:priority.top,]
    ###############
    
	if(verbose){
		now <- Sys.time()
		message(sprintf("Load positional information for Genes (%s) ...", as.character(now)), appendLF=TRUE)
	}
    gr_Gene <- xRDataLoader(RData.customised=GR.Gene[1], verbose=verbose, RData.location=RData.location)
    if(is.null(gr_Gene)){
    	GR.Gene <- "UCSC_knownGene"
		if(verbose){
			message(sprintf("Instead, %s will be used", GR.Gene), appendLF=TRUE)
		}
    	gr_Gene <- xRDataLoader(RData.customised=GR.Gene, verbose=verbose, RData.location=RData.location)
    }
    
    ## ONLY restricted to genes with genomic locations
	ind <- match(rownames(df_priority), names(gr_Gene))
	p_gr <- gr_Gene[ind[!is.na(ind)],]
	p_matrix <- df_priority[!is.na(ind),]
	
	## append genomic locations to GR object
	gr <- p_gr
	GenomicRanges::mcols(gr) <- cbind(GenomicRanges::mcols(gr), p_matrix)
	
	########################################
	if(!is.null(target.query)){
		ind <- match(target.query, names(gr))
		if(length(ind) == 0 | is.na(ind)){
			warning("The top target gene will be displayed instead!")
			target.query <- names(gr[1])
		}
	}else{
		target.query <- names(gr[1])
	}
	########################################
	
	## for ideogram track
	chr <- as.character(unique(GenomicRanges::seqnames(gr[target.query])))
	itrack <- Gviz::IdeogramTrack(genome="hg19", chromosome=chr, showBandId=TRUE, cex.bands=0.8)
	
	if(is.null(nearby)){
		## a window (eg 1e6) of upstream and downstream from the gene in query
		q2r <- as.matrix(as.data.frame(suppressWarnings(GenomicRanges::findOverlaps(query=gr, subject=gr[target.query], maxgap=window, minoverlap=1L, type="any", select="all", ignore.strand=TRUE))))
		gr_sub <- gr[q2r[,1],]
		
	}else{
		dists <- as.matrix(as.data.frame(suppressWarnings(GenomicRanges::distance(x=gr, y=gr[target.query], select="all", ignore.strand=TRUE))))
		x <- sort(dists)
		gr_sub <- gr[which(dists <= x[min(nearby+1,length(x))])]
	}
	
	## for annotation track
	gr_sub$group <- gr_sub$Symbol
	gr_sub$query <- factor(ifelse(gr_sub$Symbol==target.query, "q", "notq"))
	
	## plot tracks
	x <- NULL
	gtrack <- Gviz::GenomeAxisTrack()
	atrack <- Gviz::AnnotationTrack(gr_sub, name="Target genes", feature=gr_sub$query, shape="box", col="transparent", fill="grey")
	dtrack <- Gviz::DataTrack(gr_sub, data="priority", baseline=0, type="h", name="Priority", strand="+", background.title="transparent", background.panel="#FFFEDB", , cex.axis=0.7, col.title="blue",col.axis="grey",col.baseline="transparent",col.line="blue",lwd=2,transformation=function(x) x)
	if(query.highlight){
		res <- Gviz::plotTracks(list(itrack,gtrack,dtrack,atrack), groupAnnotation="Symbol", q="salmon", notq="grey", cex.title=0.8)
	}else{
		res <- Gviz::plotTracks(list(itrack,gtrack,dtrack,atrack), groupAnnotation="Symbol", cex.title=1)
	}
	#availableDisplayPars(dtrack)
    
    invisible(res)
}


