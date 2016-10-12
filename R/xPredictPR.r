#' Function to assess the prediction performance via Precision-Recall (PR) analysis
#'
#' \code{xPredictPR} is supposed to assess the prediction performance via Precision-Recall (PR) analysis. It requires two inputs: 1) Gold Standard Positive (GSP) containing targets; 2) prediction containing predicted targets and predictive scores.
#'
#' @param GSP a vector containing Gold Standard Positive (GSP)
#' @param prediction a data frame containing predictions along with predictive scores. It has two columns: 1st column for target, 2nd column for predictive scores (the higher the better)
#' @param num.threshold an integer to specify how many PR points (as a function of the score threshold) will be calculated
#' @param bin how to bin the scores. It can be "uniform" for binning scores with equal interval (ie with uniform distribution), and 'quantile' for binning scores with eual frequency (ie with equal number)
#' @param recall.prediction logical to indicate whether the calculation of recall is based on predictable GSP. By default, it sets to FALSE
#' @param GSN a vector containing Gold Standard Negative (GSN). It is optional. By default (NULL), GSN is not provided
#' @param plot logical to indicate whether to return an object of class "ggplot" for plotting PR curve. By default, it sets to FALSE. If TRUE, it will return a ggplot object after being appended with 'PR' and 'Fmax'
#' @param smooth logical to indicate whether to smooth the curve by making sure a non-increasing order for precision. By default, it sets to FALSE
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to TRUE for display
#' @return 
#' If plot is FALSE (by default), a data frame containing three columns: 1st column 'Threshold' for the score threshold, 2nd column 'Precision' for precision, 3rd 'Recall' for recall.
#' If plot is TRUE, it will return a ggplot object after being appended with 'PR' (a data frame containing three columns: 1st column 'Threshold' for the score threshold, 2nd column 'Precision' for precision, 3rd 'Recall' for recall), and 'Fmax' for maximum F-measure.
#' @note
#' F-measure: the maximum of a harmonic mean between precision and recall along PR curve
#' @export
#' @include xPredictPR.r
#' @examples
#' \dontrun{
#' PR <- xPredictPR(GSP, prediction)
#' }

xPredictPR <- function(GSP, prediction, num.threshold=20, bin=c("quantile", "uniform"), recall.prediction=FALSE, GSN=NULL, plot=TRUE, smooth=FALSE, verbose=TRUE)
{

    ## match.arg matches arg against a table of candidate values as specified by choices, where NULL means to take the first one
    bin <- match.arg(bin)
    
    gsp <- unique(GSP)
    if(verbose){
        now <- Sys.time()
        message(sprintf("There are %d targets in GSP (%s).", length(gsp), as.character(now)), appendLF=TRUE)
    }
    
    res_ls <- split(x=as.numeric(prediction[,2]), f=prediction[,1])
    pred <- unlist(lapply(res_ls, max))
    if(verbose){
        now <- Sys.time()
        message(sprintf("There are %d targets in predictions (%s).", length(pred), as.character(now)), appendLF=TRUE)
    }
    
	## GSP but only predicted
    ind <- match(gsp, names(pred))
    gsp_predicted <- gsp[!is.na(ind)]
    if(verbose){
        now <- Sys.time()
        message(sprintf("There are %d targets both in GSP and predictions (%s).", length(gsp_predicted), as.character(now)), appendLF=TRUE)
    }
    
    if(!is.null(GSN)){
    	ind <- match(names(pred), union(gsp_predicted,GSN))
    	pred <- pred[!is.na(ind)]
		if(verbose){
			now <- Sys.time()
			message(sprintf("Evaluation restricted to %d GSP/GSN targets (%s).", length(pred), as.character(now)), appendLF=TRUE)
		}
    }
    
    ######################################

    ## get all decision threshold
    if(bin=='uniform'){
        max_pred <- base::max(pred)
        min_pred <- base::min(pred)
        t <- base::seq(from=max_pred, to=min_pred, length.out=num.threshold+1)
    }else if(bin=='quantile'){
        t <- as.vector( stats::quantile(x=pred, probs=base::seq(from=1, to=0, length.out=num.threshold+1)) )
    }
    
    ## calcualte precision and recall
    t <- t[-1]
    res <- sapply(t, function(x){
        ### predicted targets with score greater than or equal to t
        ind <- which(pred>=x)
        callP <- length(ind)
        ### predicted targets (with score greater than or equal to t) overlapped in GSP
        ind2 <- match(names(ind), gsp)
        TP <- sum(!is.na(ind2))
        
        return(rbind(TP,callP))
    })

    x_pr <- res[1,] / res[2,]
    if(recall.prediction){
    	x_rc <- res[1,] / length(gsp_predicted)
    }else{
    	x_rc <- res[1,] / length(gsp)
    }
    ######################################
	
	if(smooth){
		## make sure the curve is smooth
		### x_pr: non-increasing order
		for(i in 2:length(x_pr)){
			if(x_pr[i] > x_pr[i-1]){
				x_pr[i] <- x_pr[i-1]
			}
		}
	}
	
    ## F-measure: the maximum (over all thresholds t) of a harmonic mean between precision and recall
    Fmeasure <- (2 * x_pr * x_rc) / (x_pr + x_rc)
    Fmax <- base::max(Fmeasure, na.rm=TRUE)
    i <- which(Fmeasure==Fmax)[1]
    
    ##############################################################################################
    
    if(verbose){
        message(sprintf("In summary, Prediction coverage: %.2f (amongst %d targets in GSP), and Fmax: %.3f.", max(x_rc), length(gsp), Fmax), appendLF=TRUE)
    }

    PR <- data.frame(Threshold=t, Precision=x_pr, Recall=x_rc)
    
    if(plot){
    	Recall <- ''
    	Precision <- ''
		p <- ggplot(PR, aes(x=Recall,y=Precision)) 
		p <- p + geom_line() + theme_bw() + ylab("Precision = TP/(TP+FP)") + xlab("Recall = TP/(TP+FN)") + ylim(0,max(PR$Precision)) + xlim(0,max(PR$Recall)) + ggtitle('PR curve') + geom_point(data=PR[i,], aes(x=Recall,y=Precision), colour="red")
		p <- p + geom_text_repel(data=PR[i,], family="Times New Roman", aes(x=Recall, y=Precision, label=paste0('Fmax = MAX{2*P*R/(P+R)} = ',signif(Fmax,digits=3))))
		p$PR <- PR
		p$Fmax <- Fmax
		
		invisible(p)
    }else{
    	invisible(PR)
    }
}