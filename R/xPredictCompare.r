#' Function to compare prediction performance results
#'
#' \code{xPredictCompare} is supposed to compare prediction performance results. It returns an object of class "ggplot".
#'
#' @param list_pPerf a list of "pPerf" objects
#' @param displayBy which curve will be used for comparison. It can be "ROC" for ROC curve (by default), "PR" for PR curve
#' @param sort logical to indicate whether to sort methods according to performance. By default, it sets TRUE
#' @param detail logical to indicate whether to label methods along with performance. By default, it sets TRUE
#' @param facet logical to indicate whether to facet/wrap a 1d of panels into 2d. By default, it sets FALSE
#' @param signature a logical to indicate whether the signature is assigned to the plot caption. By default, it sets TRUE showing which function is used to draw this graph
#' @return an object of class "ggplot" or NULL (if all input pPerf objects are NULL)
#' @note none
#' @export
#' @seealso \code{\link{xPredictROCR}}
#' @include xPredictCompare.r
#' @examples
#' # Load the library
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#' RData.location <- "http://galahad.well.ox.ac.uk/bigdata_dev"
#' \dontrun{
#' bp <- xPredictCompare(ls_pPerf, displayBy="ROC")
#' print(bp)
#' ## modify legend position
#' bp + theme(legend.position=c(0.75,0.25))
#' }

xPredictCompare <- function(list_pPerf, displayBy=c("ROC","PR"), sort=TRUE, detail=TRUE, facet=FALSE, signature=TRUE)
{

    displayBy <- match.arg(displayBy)
    
   	if(any(class(list_pPerf) %in% c("gg","ggplot","pPerf"))){
		list_pPerf <- list(list_pPerf)
	}else if(class(list_pPerf)=="list"){
		## Remove null elements in a list
		list_pPerf <- base::Filter(base::Negate(is.null), list_pPerf)
		if(length(list_pPerf)==0){
			warnings("All pPerf objects are NULL!")
			return(NULL)
		}
	}else{
		stop("The function must apply to an 'list' object, or an 'pPerf'/'ggplot' object.\n")
	}
    
	## Combine into a data frame called 'df_PRS'
	list_names <- names(list_pPerf)
	if(is.null(list_names)){
		list_names <- paste('Method', 1:length(list_pPerf), sep=' ')
	}
	ls_prs <- lapply(1:length(list_pPerf), function(i){
		prs <- list_pPerf[[i]]$PRS
		fmax <- signif(list_pPerf[[i]]$Fmax, digits=3)
		auroc <- signif(list_pPerf[[i]]$AUROC, digits=3)
		method <- list_names[i]
		
		#label <- paste(method, ' (AUC=', auroc, '; Fmax=', fmax,')', sep='')
		if(displayBy=='ROC'){
			label <- paste(method, ' (AUC=', auroc, ')', sep='')
		}else if(displayBy=='PR'){
			label <- paste(method, ' (Fmax=', fmax,')', sep='')
		}
		
		cbind(prs, Method=rep(method,nrow(prs)), fmax=rep(fmax,nrow(prs)), auroc=rep(auroc,nrow(prs)), Label=rep(label,nrow(prs)), stringsAsFactors=FALSE)
	})
	df_PRS <- do.call(rbind, ls_prs)

	## Method factor
	df_PRS$Method <- factor(df_PRS$Method, levels=list_names)
	
	## draw curves
    Recall <- ''
    Precision <- ''
    Specificity <- ''
    Method <- ''
    Label <- ''
    auroc <- ''
	if(displayBy=='ROC'){
		## sort by: auroc
		if(sort){
			df_PRS <- df_PRS[with(df_PRS, order(-auroc)), ]
			## define levels
			if(detail){
				df_PRS$Label <- factor(df_PRS$Label, levels=unique(df_PRS$Label))
			}else{
				df_PRS$Method <- factor(df_PRS$Method, levels=unique(df_PRS$Method))
			}
		}
		## ggplot
		p <- ggplot(df_PRS, aes(x=1-Specificity,y=Recall))
		
		if(detail){
			p <- p + geom_line(aes(colour=factor(Label)))
		}else{
			p <- p + geom_line(aes(colour=factor(Method)))
		}
		
		p <- p + ylab("True Positive Rate = TP/(TP+FN)") + xlab("False Positive Rate = FP/(FP+TN)") + ylim(0,max(df_PRS$Recall)) + xlim(0,max(1-df_PRS$Specificity))

	}else if(displayBy=='PR'){
		## sort by: fmax
		fmax <- ''
		if(sort){
			df_PRS <- df_PRS[with(df_PRS, order(-fmax)), ]
			## define levels
			if(detail){
				df_PRS$Label <- factor(df_PRS$Label, levels=unique(df_PRS$Label))
			}else{
				df_PRS$Method <- factor(df_PRS$Method, levels=unique(df_PRS$Method))
			}
		}
		## ggplot
		p <- ggplot(df_PRS, aes(x=Recall,y=Precision)) 

		if(detail){
			p <- p + geom_line(aes(colour=factor(Label)))
		}else{
			p <- p + geom_line(aes(colour=factor(Method)))
		}

		p <- p + ylab("Precision = TP/(TP+FP)") + xlab("Recall = TP/(TP+FN)") + ylim(0,max(df_PRS$Precision)) + xlim(0,max(df_PRS$Recall))
		
	}
	
	p <- p + theme_bw() + theme(axis.title.y=element_text(size=12,color="black"), axis.title.x=element_text(size=12,color="black"))
	
	if(facet){
		if(detail){
			p <- p + facet_wrap(~Label)
		}else{
			p <- p + facet_wrap(~Method)
		}
		
		## strip
		p <- p + theme(strip.background=element_rect(fill="transparent",color="transparent"), strip.text=element_text(face="italic"))
		
		p <- p + theme(legend.position="none", legend.title=element_blank())
	}else{
		p <- p + theme(legend.title=element_blank(), legend.key=element_rect(colour="transparent"))
		
		#p + theme(legend.position=c(0.75,0.25))
	}
	
	if(signature){
		caption <- paste("Created by xPredictROCR from Pi version", utils ::packageVersion("XGR"))
		p <- p + labs(caption=caption) + theme(plot.caption=element_text(hjust=1,face='bold.italic',size=8,colour='#002147'))
	}
	## put arrows on both axes
	p <- p + theme(axis.line=element_line(arrow=arrow(angle=30,length=unit(0.25,"cm"), type="open")))
	
	invisible(p)
}
