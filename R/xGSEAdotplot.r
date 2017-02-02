#' Function to visualise GSEA results using dot plot
#'
#' \code{xGSEAdotplot} is supposed to visualise GSEA results using dot plot. It returns an object of class "ggplot" or a list of "ggplot" objects.
#'
#' @param eGSEA an object of class "eGSEA"
#' @param top the number of the top enrichments to be visualised. Alternatively, the gene set names can be queried
#' @param priority.color a character vector for coloring priority scores
#' @param peak logical to indicate whether the peak info of leading genes is labelled
#' @param compact logical to indicate whether the compact/void theme is used. If TRUE, axes and legend info will be hidden
#' @param signature logical to indicate whether the signature is assigned to the plot caption. By default, it sets TRUE showing which function is used to draw this graph
#' @return an object of class "ggplot" or a list of "ggplot" objects.
#' @note none
#' @export
#' @seealso \code{\link{xPierGSEA}}
#' @include xGSEAdotplot.r
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' gp <- xGSEAdotplot(eGSEA, top=1)
#' #gp <- xGSEAdotplot(eGSEA, top=1, peak=FALSE, compact=TRUE, signature=FALSE)
#' gp
#' 
#' ls_gp <- xGSEAdotplot(eGSEA, top=1:4, signature=FALSE)
#' library(gridExtra)
#' grid.arrange(grobs=ls_gp, ncol=2)
#' }

xGSEAdotplot <- function(eGSEA, top=1, priority.color=c("lightyellow","orange"), peak=TRUE, compact=FALSE, signature=TRUE)
{

    if(class(eGSEA) != "eGSEA"){
    	stop("The function must apply to a 'eGSEA' object.\n")
    }
    
    df_summary <- eGSEA$df_summary
    nSet <- nrow(df_summary)
    
    ## determine which gene set
    if(class(top)=="integer" | class(top)=="numeric"){
     	top <- as.integer(top)
    	ind <- which((top <= nSet) & (top >= 1))
        if(length(ind)>0){
        	which.terms <- top[ind]
        }else{
        	which.terms <- NULL
        }
        
    }else{
        ind <- which(df_summary$setID %in% top)
        if(length(ind)>0){
        	which.terms <- ind
        }else{
        	which.terms <- NULL
        }
        
    }
    
    if(is.null(which.terms)){
    	return(NULL)
    }
    
    Hits <- Rank <- RES <- Score <- x <- NULL
    ls_gp <- lapply(which.terms, function(which.term){

		df_full <- eGSEA$full[[which.term]]
		df_leading <- subset(df_full, Hits==3)
		
		nLead <- df_summary[which.term, "nLead"]
		nes <- df_summary[which.term, "nes"]
		pvalue <- df_summary[which.term, "pvalue"]
		adjp <- df_summary[which.term, "adjp"]
		
		leading_info <- paste("Peak (rank=", df_leading$Rank, ")",
						 "\nLeading genes (n=", nLead, ")",
						 "\nSignificance (NES=", nes,
						 ", P=", pvalue,
						 ", FDR=", adjp,")",
						 sep="",collapse="")
	
		bp <- ggplot(df_full, aes(x=Rank, y=RES, colour=Score))
		bp <- bp + geom_point(size=0.1)
		bp <- bp + geom_segment(data=subset(df_full,Hits>=1), aes(xend=Rank, yend=0), size=0.2) + scale_colour_gradient(low=priority.color[1],high=priority.color[2], limits=c(min(df_full$Score),max(df_full$Score)), guide=guide_colorbar(title="Priority",barwidth=0.5,nbin=5))
		
		if(peak){
			bp <- bp + geom_point(data=df_leading, aes(x=Rank, y=RES), colour="red") + geom_segment(data=df_leading, aes(xend=Rank,yend=0), size=0.2, colour="red", linetype="dashed") + ggrepel::geom_text_repel(data=df_leading, aes(x=Rank,y=RES,label=leading_info), size=2, color='blue', alpha=0.8, fontface='bold.italic')
		}
		
		bp <- bp  + theme_bw() + theme(legend.position="right", legend.title=element_text(color="black",face="italic",size=10), axis.title.y=element_text(color="black"), axis.title.x=element_text(color="black"), panel.border=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
		bp <- bp + xlab("Gene rankings") + ylab("Running enrichment score")

		## title
		title <- paste0(df_summary[which.term,"name"], " (n=", df_summary[which.term,"nAnno"], ")")
		bp <- bp + labs(title=title) + theme(plot.title=element_text(hjust=0.5,size=12))
		## caption
		if(signature){
			caption <- paste("Created by xGSEAdotplot from Pi version", utils ::packageVersion("Pi"))
			bp <- bp + labs(caption=caption) + theme(plot.caption=element_text(hjust=1,face='bold.italic',size=8,colour='#002147'))
		}
	
		## put arrows on x- and y-axis
		gp <- bp + theme(axis.line.x=element_line(arrow=arrow(angle=30,length=unit(0.25,"cm"), type="open")), axis.line.y=element_line(arrow=arrow(angle=30,length=unit(0.25,"cm"), type="open")))
		
		# whether is compact
		if(compact){
			gp <- gp + theme_void() + theme(legend.position="none")
		}
		
		invisible(gp)
    })
    names(ls_gp) <- which.terms
    
    if(length(ls_gp)==1){
    	invisible(ls_gp[[1]])
    }else{
    	invisible(ls_gp)
    }
}
