#' Function to define eQTL genes given a list of SNPs or a customised eQTL mapping data
#'
#' \code{xSNP2eGenes} is supposed to define eQTL genes given a list of SNPs or a customised eQTL mapping data. The eQTL weight is calcualted as Cumulative Distribution Function of negative log-transformed eQTL-reported signficance level. 
#'
#' @param data a input vector containing SNPs. SNPs should be provided as dbSNP ID (ie starting with rs). Alternatively, they can be in the format of 'chrN:xxx', where N is either 1-22 or X, xxx is number; for example, 'chr16:28525386'
#' @param include.eQTL genes modulated by eQTL (also Lead SNPs or in LD with Lead SNPs) are also included. By default, it is 'NA' to disable this option. Otherwise, those genes modulated by eQTL will be included: immune stimulation in monocytes ('JKscience_TS1A' and 'JKscience_TS2B' for cis-eQTLs or 'JKscience_TS3A' for trans-eQTLs) from Science 2014, 343(6175):1246949; cis- and trans-eQTLs in B cells ('JKng_bcell') and in monocytes ('JKng_mono') from Nature Genetics 2012, 44(5):502-510; cis- and trans-eQTLs in neutrophils ('JKnc_neutro') from Nature Communications 2015, 7(6):7545; cis-eQTLs in NK cells ('JK_nk') which is unpublished. Also supported are GTEx cis-eQTLs from Science 2015, 348(6235):648-60, including 13 tissues: 'GTEx_Adipose_Subcutaneous','GTEx_Artery_Aorta','GTEx_Artery_Tibial','GTEx_Esophagus_Mucosa','GTEx_Esophagus_Muscularis','GTEx_Heart_Left_Ventricle','GTEx_Lung','GTEx_Muscle_Skeletal','GTEx_Nerve_Tibial','GTEx_Skin_Sun_Exposed_Lower_leg','GTEx_Stomach','GTEx_Thyroid','GTEx_Whole_Blood'.
#' @param eQTL.customised a user-input matrix or data frame with 3 columns: 1st column for SNPs/eQTLs, 2nd column for Genes, and 3rd for eQTL mapping significance level (p-values or FDR). It is designed to allow the user analysing their eQTL data. This customisation (if provided) has the high priority over built-in eQTL data.
#' @param cdf.function a character specifying a Cumulative Distribution Function (cdf). It can be one of 'exponential' based on exponential cdf, 'empirical' for empirical cdf
#' @param plot logical to indicate whether the histogram plot (plus density or CDF plot) should be drawn. By default, it sets to false for no plotting
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to true for display
#' @param RData.location the characters to tell the location of built-in RData files. See \code{\link{xRDataLoader}} for details
#' @return
#' a data frame with following columns:
#' \itemize{
#'  \item{\code{Gene}: eQTL-containing genes}
#'  \item{\code{SNP}: eQTLs}
#'  \item{\code{Sig}: the eQTL mapping significant level (the best/minimum)}
#'  \item{\code{Weight}: the eQTL weight}
#' }
#' @note None
#' @export
#' @seealso \code{\link{xRDataLoader}}
#' @include xSNP2eGenes.r
#' @examples
#' \dontrun{
#' # Load the library
#' library(Pi)
#' }
#'
#' # a) provide the SNPs with the significance info
#' ## get lead SNPs reported in AS GWAS and their significance info (p-values)
#' #data.file <- "http://galahad.well.ox.ac.uk/bigdata/AS.txt"
#' #AS <- read.delim(data.file, header=TRUE, stringsAsFactors=FALSE)
#' ImmunoBase <- xRDataLoader(RData.customised='ImmunoBase')
#' gr <- ImmunoBase$AS$variants
#' AS <- as.data.frame(GenomicRanges::mcols(gr)[, c('Variant','Pvalue')])
#'
#' # b) define eQTL genes
#' df_eGenes <- xSNP2eGenes(data=AS[,1], include.eQTL="JKscience_TS2A")

xSNP2eGenes <- function(data, include.eQTL=c(NA,"JKscience_TS2A","JKscience_TS2B","JKscience_TS3A","JKng_bcell","JKng_mono","JKnc_neutro","JK_nk", "GTEx_V4_Adipose_Subcutaneous","GTEx_V4_Artery_Aorta","GTEx_V4_Artery_Tibial","GTEx_V4_Esophagus_Mucosa","GTEx_V4_Esophagus_Muscularis","GTEx_V4_Heart_Left_Ventricle","GTEx_V4_Lung","GTEx_V4_Muscle_Skeletal","GTEx_V4_Nerve_Tibial","GTEx_V4_Skin_Sun_Exposed_Lower_leg","GTEx_V4_Stomach","GTEx_V4_Thyroid","GTEx_V4_Whole_Blood","eQTLdb_NK","eQTLdb_CD14","eQTLdb_LPS2","eQTLdb_LPS24","eQTLdb_IFN"), eQTL.customised=NULL, cdf.function=c("empirical","exponential"), plot=FALSE, verbose=TRUE, RData.location="https://github.com/hfang-bristol/RDataCentre/blob/master/Portal")
{

    ## match.arg matches arg against a table of candidate values as specified by choices, where NULL means to take the first one
    cdf.function <- match.arg(cdf.function)

	## replace '_' with ':'
	data <- gsub("_", ":", data, perl=TRUE)
	## replace 'imm:' with 'chr'
	data <- gsub("imm:", "chr", data, perl=TRUE)
    
    data <- unique(data)
    
	if(verbose){
		now <- Sys.time()
		message(sprintf("A total of %d SNPs are input", length(data)), appendLF=TRUE)
	}
    
    ######################################################
    # Link to targets based on eQTL
    ######################################################
    df_SGS <- xSNPeqtl(data=NULL, include.eQTL=include.eQTL, eQTL.customised=eQTL.customised, verbose=verbose, RData.location=RData.location)
	
	if(!is.null(df_SGS)){	
		
		uid <- paste(df_SGS[,1], df_SGS[,2], sep='_')
		df <- cbind(uid, df_SGS)
		res_list <- split(x=df$Sig, f=df$uid)
		vec <- unlist(lapply(res_list, min))
		raw_score <- -1*log10(vec)
		
		if(cdf.function == "exponential"){
			##  fit raw_score to the cumulative distribution function (CDF; depending on exponential empirical distributions)
			lambda <- MASS::fitdistr(raw_score, "exponential")$estimate
		
			## eQTL weight for input SNPs
			ind <- match(df_SGS[,1], data)
			df <- data.frame(df_SGS[!is.na(ind),])
			## weights according to eQTL
			wE <- stats::pexp(-log10(df$Sig), rate=lambda)
			
			#########
			if(nrow(df)==0){
				df_eGenes <- NULL
			}else{
				df_eGenes <- data.frame(Gene=df$Gene, SNP=df$SNP, Sig=df$Sig, Weight=wE, row.names=NULL, stringsAsFactors=FALSE)
			}
			#########
			
			if(plot){
				hist(raw_score, breaks=1000, freq=FALSE, col="grey", xlab="-log10(p-values)", main="")
				curve(stats::dexp(x=raw_score,rate=lambda), 0:max(raw_score), col=2, add=TRUE)
			}
			
			if(verbose){
				now <- Sys.time()
				message(sprintf("eQTL weights are CDF of exponential empirical distributions (parameter lambda=%f)", lambda), appendLF=TRUE)
			}
			
		}else if(cdf.function == "empirical"){
			## Compute an empirical cumulative distribution function
			my.CDF <- stats::ecdf(raw_score)
			
			## eQTL weight for input SNPs
			ind <- match(df_SGS[,1], data)
			df <- data.frame(df_SGS[!is.na(ind),])
			## weights according to eQTL
			wE <- my.CDF(-log10(df$Sig))
			
			#########
			if(nrow(df)==0){
				df_eGenes <- NULL
			}else{
				df_eGenes <- data.frame(Gene=df$Gene, SNP=df$SNP, Sig=df$Sig, Weight=wE, row.names=NULL, stringsAsFactors=FALSE)
				df_eGenes <- df_eGenes[order(df_eGenes$Gene,df_eGenes$Sig,df_eGenes$SNP,decreasing=FALSE),]
			}
			#########
			
			if(plot){
				plot(my.CDF, xlab="-log10(p-values)", ylab="Empirical CDF (eQTL weights)", main="")
			}
			
			if(verbose){
				now <- Sys.time()
				message(sprintf("eQTL weights are CDF of empirical distributions"), appendLF=TRUE)
			}
			
		}
	
		if(verbose){
			now <- Sys.time()
			message(sprintf("%d nGenes are defined involving %d eQTL", length(unique(df_eGenes$Gene)), length(unique(df_eGenes$SNP))), appendLF=TRUE)
		}
	
	}else{
		df_eGenes <- NULL
		
		if(verbose){
			now <- Sys.time()
			message(sprintf("No eQTL genes are defined"), appendLF=TRUE)
		}
	}
	
    invisible(df_eGenes)
}
