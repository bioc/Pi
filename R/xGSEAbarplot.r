#' Function to visualise GSEA results using a barplot
#'
#' \code{xGSEAbarplot} is supposed to visualise GSEA results using a barplot. It returns an object of class "ggplot".
#'
#' @param eGSEA an object of class "eGSEA"
#' @param top_num the number of the top terms (sorted according to FDR or adjusted p-values). If it is 'auto', only the significant terms (see below FDR.cutoff) will be displayed
#' @param displayBy which statistics will be used for displaying. It can be "nes" for normalised enrichment score (by default), "adjp" or "fdr" for adjusted p value (or FDR), "pvalue" for p value
#' @param FDR.cutoff FDR cutoff used to declare the significant terms. By default, it is set to 0.05. This option only works when setting top_num (see above) is 'auto'
#' @param bar.label logical to indicate whether to label each bar with FDR. By default, it sets to true for bar labelling
#' @param bar.label.size an integer specifying the bar labelling text size. By default, it sets to 3
#' @param wrap.width a positive integer specifying wrap width of name
#' @param signature logical to indicate whether the signature is assigned to the plot caption. By default, it sets TRUE showing which function is used to draw this graph
#' @return an object of class "ggplot"
#' @note none
#' @export
#' @seealso \code{\link{xPierGSEA}}
#' @include xGSEAbarplot.r
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' bp <- xGSEAbarplot(eGSEA, top_num="auto", displayBy="nes")
#' #pdf(file="GSEA_barplot.pdf", height=6, width=12, compress=TRUE)
#' print(bp)
#' #dev.off()
#' }

xGSEAbarplot <- function(eGSEA, top_num=10, displayBy=c("nes","adjp","fdr","pvalue"), FDR.cutoff=0.05, bar.label=TRUE, bar.label.size=3, wrap.width=NULL, signature=TRUE) 
{
    
    displayBy <- match.arg(displayBy)
    
    if(class(eGSEA) != "eGSEA"){
    	stop("The function must apply to a 'eGSEA' object.\n")
    }
    df_summary <- eGSEA$df_summary
    
    ## when 'auto', will keep the significant terms
	if(top_num=='auto'){
		top_num <- sum(df_summary$adjp<FDR.cutoff)
		if(top_num <= 1){
			top_num <- 10
		}
	}
	if(top_num > nrow(df_summary)){
		top_num <- nrow(df_summary)
	}
	df <- df_summary[1:top_num, ]
	
	## text wrap
	if(!is.null(wrap.width)){
		width <- as.integer(wrap.width)
		res_list <- lapply(df$name, function(x){
			x <- gsub('_', ' ', x)
			y <- strwrap(x, width=width)
			if(length(y)>1){
				paste0(y[1], '...')
			}else{
				y
			}
		})
		df$name <- unlist(res_list)
	}
	
	adjp <- nes <- pvalue <- NULL
	name <- height <- NULL
	if(displayBy=='adjp' | displayBy=='fdr'){
		df <- df[with(df,order(-adjp,nes)),]
		df$name <- factor(df$name, levels=df$name)
		df$height <- -1*log10(df$adjp)
		p <- ggplot(df, aes(x=name, y=height))
		p <- p + ylab("Enrichment significance: -log10(FDR)")
	}else if(displayBy=='nes'){
		df <- df[with(df,order(nes,-pvalue)),]
		df$name <- factor(df$name, levels=df$name)
		df$height <- df$nes
		p <- ggplot(df, aes(x=name, y=height))
		p <- p + ylab("Normalised enrichment score (NES)")
	}else if(displayBy=='pvalue'){
		df <- df[with(df,order(-pvalue,nes)),]
		df$name <- factor(df$name, levels=df$name)
		df$height <- -1*log10(df$pvalue)
		p <- ggplot(df, aes(x=name, y=height))
		p <- p + ylab("Enrichment significance: -log10(p-value)")
	}
	
	bp <- p + geom_col(aes(fill=height)) + scale_fill_gradient(low="lightyellow",high="orange") + theme_bw() + theme(legend.position="none",axis.title.y=element_blank(), axis.text.y=element_text(size=12,color="black"), axis.title.x=element_text(size=14,color="black")) + coord_flip()
	
	if(bar.label){
		## get text label
		to_scientific_notation <- function(x) {
			res <- format(x, scientific=TRUE)
			res <- sub("\\+0?", "", res)
			sub("-0?", "-", res)
		}
		label <- to_scientific_notation(df$adjp)
		label <- paste('FDR', as.character(label), sep='=')
		
		bp <- bp + geom_text(aes(label=label),hjust=1,size=bar.label.size)
	}
	
	## caption
    if(signature){
    	caption <- paste("Created by xGSEAbarplot from Pi version", utils ::packageVersion("Pi"))
    	bp <- bp + labs(caption=caption) + theme(plot.caption=element_text(hjust=1,face='bold.italic',size=8,colour='#002147'))
    }
	
	## put arrows on x-axis
	bp <- bp + theme(axis.line.x=element_line(arrow=arrow(angle=30,length=unit(0.25,"cm"), type="open")))
	
	## x-axis (actually y-axis) position
	bp <- bp + scale_y_continuous(position="top")
	
	invisible(bp)
}