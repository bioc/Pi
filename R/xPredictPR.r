#' Function to assess the prediction performance via Precision-Recall (PR) analysis
#'
#' \code{xPredictPR} is supposed to assess the prediction performance via Precision-Recall (PR) analysis. It requires two inputS: 1) Glod Standard Positive (GSP) containing targets; 2) prediction containing predicted targets and predictive scores.
#'
#' @param GSP a vector containing Glod Standard Positive (GSP)
#' @param prediction a data frame containing predictions along with predictive scores. It has two columns: 1st column for target, 2nd column for predictive scores (the higher the better)
#' @param num.threshold an integer to specify how many PR points (as a function of the score threshold) will be calculated
#' @param bin how to bin the scores. It can be "uniform" for binning scores with equal interval (ie with uniform distribution), and 'quantile' for binning scores with eual frequency (ie with equal number)
#' @param recall.prediction logical to indicate whether the calculation of recall is based on preditable GSP. By default, it sets to FALSE
#' @param plot logical to indicate whether to return an object of class "ggplot" for plotting PR curve. By default, it sets to FALSE. If TRUE, it will return a ggplot object after being appended with 'PR'
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to TRUE for display
#' @return 
#' If plot is FALSE (by default), a data frame containing two columns: 1st column 'Precision' for precision, 2nd 'Recall' for recall. The row has the names corresponding to the score threshold.
#' If plot is TRUE, it will return a ggplot object after being appended with 'PR' (a data frame containing two columns: 1st column 'Precision' for precision, 2nd 'Recall' for recall. The row has the names corresponding to the score threshold).
#' @note
#' F-measure: the maximum of a harmonic mean between precision and recall along PR curve
#' @export
#' @include xPredictPR.r
#' @examples
#' \dontrun{
#' PR <- xPredictPR(GSP, prediction)
#' }

xPredictPR <- function(GSP, prediction, num.threshold=100, bin=c("quantile", "uniform"), recall.prediction=FALSE, plot=FALSE, verbose=TRUE)
{

    ## match.arg matches arg against a table of candidate values as specified by choices, where NULL means to take the first one
    bin <- match.arg(bin)
    
    gsp <- unique(GSP)
    if(verbose){
        now <- Sys.time()
        message(sprintf("There are %d targets in GSP (%s).", length(gsp), as.character(now)), appendLF=TRUE)
    }
    
    res_ls <- split(x=prediction[,2], f=prediction[,1])
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
    res <- sapply(t, function(x){
        ### predicted targets with score greater than or equal to t
        ind <- which(pred>=x)
        callP <- length(ind)
        ### redicted targets (with score greater than or equal to t) overlapped in GSP
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

    ## F-measure: the maximum (over all thresholds t) of a harmonic mean between precision and recall
    Fmeasure <- base::max( (2 * x_pr * x_rc) / (x_pr + x_rc), na.rm=TRUE)
    
    ##############################################################################################
    
    if(verbose){
        message(sprintf("In summary, Prediction coverage: %.2f (amongst %d targets in GSP), and F-measure: %.2f.", max(x_rc), length(gsp), Fmeasure), appendLF=TRUE)
    }

    PR <- data.frame(Precision=x_pr, Recall=x_rc, row.names=TRUE)
    
    if(plot){
		p <- ggplot(PR, aes(x=PR$Recall, y=PR$Precision)) 
		p <- p + geom_line() + geom_point() + theme_bw() + ylab("Precision") + xlab("Recall") + ylim(0,1) + xlim(0,1) + ggtitle(paste0('F-measure: ', signif(Fmeasure,digits=3)))
		p$PR <- PR
		
		invisible(p)
    }else{
    	invisible(PR)
    }
}