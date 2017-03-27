#' Function to define eQTL genes given a list of SNPs or a customised eQTL mapping data
#'
#' \code{xSNP2eGenes} is supposed to define eQTL genes given a list of SNPs or a customised eQTL mapping data. The eQTL weight is calcualted as Cumulative Distribution Function of negative log-transformed eQTL-reported signficance level. 
#'
#' @param data a input vector containing SNPs. SNPs should be provided as dbSNP ID (ie starting with rs). Alternatively, they can be in the format of 'chrN:xxx', where N is either 1-22 or X, xxx is number; for example, 'chr16:28525386'
#' @param include.eQTL genes modulated by eQTL (also Lead SNPs or in LD with Lead SNPs) are also included. By default, it is 'NA' to disable this option. Otherwise, those genes modulated by eQTL will be included. Pre-built eQTL datasets are detailed in the section 'Note'
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
#' @note Pre-built eQTL datasets are described below according to the data sources.\cr
#' 1. Context-specific eQTLs in monocytes: resting and activating states. Sourced from Science 2014, 343(6175):1246949
#' \itemize{
#'  \item{\code{JKscience_TS2A}: cis-eQTLs in either state (based on 228 individuals with expression data available for all experimental conditions).}
#'  \item{\code{JKscience_TS2A_CD14}: cis-eQTLs only in the resting/CD14+ state (based on 228 individuals).}
#'  \item{\code{JKscience_TS2A_LPS2}: cis-eQTLs only in the activating state induced by 2-hour LPS (based on 228 individuals).}
#'  \item{\code{JKscience_TS2A_LPS24}: cis-eQTLs only in the activating state induced by 24-hour LPS (based on 228 individuals).}
#'  \item{\code{JKscience_TS2A_IFN}: cis-eQTLs only in the activating state induced by 24-hour interferon-gamma (based on 228 individuals).}
#'  \item{\code{JKscience_TS2B}: cis-eQTLs in either state (based on 432 individuals).}
#'  \item{\code{JKscience_TS2B_CD14}: cis-eQTLs only in the resting/CD14+ state (based on 432 individuals).}
#'  \item{\code{JKscience_TS2B_LPS2}: cis-eQTLs only in the activating state induced by 2-hour LPS (based on 432 individuals).}
#'  \item{\code{JKscience_TS2B_LPS24}: cis-eQTLs only in the activating state induced by 24-hour LPS (based on 432 individuals).}
#'  \item{\code{JKscience_TS2B_IFN}: cis-eQTLs only in the activating state induced by 24-hour interferon-gamma (based on 432 individuals).}
#'  \item{\code{JKscience_TS3A}: trans-eQTLs in either state.}
#'  \item{\code{JKscience_CD14}: cis and trans-eQTLs in the resting/CD14+ state (based on 228 individuals).}
#'  \item{\code{JKscience_LPS2}: cis and trans-eQTLs in the activating state induced by 2-hour LPS (based on 228 individuals).}
#'  \item{\code{JKscience_LPS24}: cis and trans-eQTLs in the activating state induced by 24-hour LPS (based on 228 individuals).}
#'  \item{\code{JKscience_IFN}: cis and trans-eQTLs in the activating state induced by 24-hour interferon-gamma (based on 228 individuals).}
#' }
#' 2. eQTLs in B cells. Sourced from Nature Genetics 2012, 44(5):502-510
#' \itemize{
#'  \item{\code{JKng_bcell}: cis- and trans-eQTLs.}
#'  \item{\code{JKng_bcell_cis}: cis-eQTLs only.}
#'  \item{\code{JKng_bcell_trans}: trans-eQTLs only.}
#' }
#' 3. eQTLs in monocytes. Sourced from Nature Genetics 2012, 44(5):502-510
#' \itemize{
#'  \item{\code{JKng_mono}: cis- and trans-eQTLs.}
#'  \item{\code{JKng_mono_cis}: cis-eQTLs only.}
#'  \item{\code{JKng_mono_trans}: trans-eQTLs only.}
#' }
#' 4. eQTLs in neutrophils. Sourced from Nature Communications 2015, 7(6):7545
#' \itemize{
#'  \item{\code{JKnc_neutro}: cis- and trans-eQTLs.}
#'  \item{\code{JKnc_neutro_cis}: cis-eQTLs only.}
#'  \item{\code{JKnc_neutro_trans}: trans-eQTLs only.}
#' }
#' 5. eQTLs in NK cells. Unpublished
#' \itemize{
#'  \item{\code{JK_nk}: cis- and trans-eQTLs.}
#'  \item{\code{JK_nk_cis}: cis-eQTLs only.}
#'  \item{\code{JK_nk_trans}: trans-eQTLs only.}
#' }
#' 6. Tissue-specific eQTLs from GTEx (version 4; incuding 13 tissues). Sourced from Science 2015, 348(6235):648-60
#' \itemize{
#'  \item{\code{GTEx_V4_Adipose_Subcutaneous}: cis-eQTLs in tissue 'Adipose Subcutaneous'.}
#'  \item{\code{GTEx_V4_Artery_Aorta}: cis-eQTLs in tissue 'Artery Aorta'.}
#'  \item{\code{GTEx_V4_Artery_Tibial}: cis-eQTLs in tissue 'Artery Tibial'.}
#'  \item{\code{GTEx_V4_Esophagus_Mucosa}: cis-eQTLs in tissue 'Esophagus Mucosa'.}
#'  \item{\code{GTEx_V4_Esophagus_Muscularis}: cis-eQTLs in tissue 'Esophagus Muscularis'.}
#'  \item{\code{GTEx_V4_Heart_Left_Ventricle}: cis-eQTLs in tissue 'Heart Left Ventricle'.}
#'  \item{\code{GTEx_V4_Lung}: cis-eQTLs in tissue 'Lung'.}
#'  \item{\code{GTEx_V4_Muscle_Skeletal}: cis-eQTLs in tissue 'Muscle Skeletal'.}
#'  \item{\code{GTEx_V4_Nerve_Tibial}: cis-eQTLs in tissue 'Nerve Tibial'.}
#'  \item{\code{GTEx_V4_Skin_Sun_Exposed_Lower_leg}: cis-eQTLs in tissue 'Skin Sun Exposed Lower leg'.}
#'  \item{\code{GTEx_V4_Stomach}: cis-eQTLs in tissue 'Stomach'.}
#'  \item{\code{GTEx_V4_Thyroid}: cis-eQTLs in tissue 'Thyroid'.}
#'  \item{\code{GTEx_V4_Whole_Blood}: cis-eQTLs in tissue 'Whole Blood'.}
#' }
#' 7. eQTLs in CD4 T cells. Sourced from PLoS Genetics 2017
#' \itemize{
#'  \item{\code{JKpg_CD4}: cis- and trans-eQTLs.}
#'  \item{\code{JKpg_CD4_cis}: cis-eQTLs only.}
#'  \item{\code{JKpg_CD4_trans}: trans-eQTLs only.}
#' }
#' 8. eQTLs in CD8 T cells. Sourced from PLoS Genetics 2017
#' \itemize{
#'  \item{\code{JKpg_CD8}: cis- and trans-eQTLs.}
#'  \item{\code{JKpg_CD8_cis}: cis-eQTLs only.}
#'  \item{\code{JKpg_CD8_trans}: trans-eQTLs only.}
#' }
#' 9. eQTLs in blood. Sourced from Nature Genetics 2013, 45(10):1238-1243
#' \itemize{
#'  \item{\code{WESTRAng_blood}: cis- and trans-eQTLs.}
#'  \item{\code{WESTRAng_blood_cis}: cis-eQTLs only.}
#'  \item{\code{WESTRAng_blood_trans}: trans-eQTLs only.}
#' }
#' @export
#' @seealso \code{\link{xRDataLoader}}
#' @include xSNP2eGenes.r
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
#' # b) define eQTL genes
#' df_eGenes <- xSNP2eGenes(data=AS[,1], include.eQTL="JKscience_TS2A", RData.location=RData.location)
#' }

xSNP2eGenes <- function(data, include.eQTL=c(NA,"JKscience_CD14","JKscience_LPS2","JKscience_LPS24","JKscience_IFN","JKscience_TS2A","JKscience_TS2A_CD14","JKscience_TS2A_LPS2","JKscience_TS2A_LPS24","JKscience_TS2A_IFN","JKscience_TS2B","JKscience_TS2B_CD14","JKscience_TS2B_LPS2","JKscience_TS2B_LPS24","JKscience_TS2B_IFN","JKscience_TS3A","JKng_bcell","JKng_bcell_cis","JKng_bcell_trans","JKng_mono","JKng_mono_cis","JKng_mono_trans","JKpg_CD4","JKpg_CD4_cis","JKpg_CD4_trans","JKpg_CD8","JKpg_CD8_cis","JKpg_CD8_trans","JKnc_neutro","JKnc_neutro_cis","JKnc_neutro_trans","WESTRAng_blood","WESTRAng_blood_cis","WESTRAng_blood_trans","JK_nk","JK_nk_cis","JK_nk_trans", "GTEx_V4_Adipose_Subcutaneous","GTEx_V4_Artery_Aorta","GTEx_V4_Artery_Tibial","GTEx_V4_Esophagus_Mucosa","GTEx_V4_Esophagus_Muscularis","GTEx_V4_Heart_Left_Ventricle","GTEx_V4_Lung","GTEx_V4_Muscle_Skeletal","GTEx_V4_Nerve_Tibial","GTEx_V4_Skin_Sun_Exposed_Lower_leg","GTEx_V4_Stomach","GTEx_V4_Thyroid","GTEx_V4_Whole_Blood","eQTLdb_NK","eQTLdb_CD14","eQTLdb_LPS2","eQTLdb_LPS24","eQTLdb_IFN"), eQTL.customised=NULL, cdf.function=c("empirical","exponential"), plot=FALSE, verbose=TRUE, RData.location="http://galahad.well.ox.ac.uk/bigdata")
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
			message(sprintf("%d eGenes are defined involving %d eQTL", length(unique(df_eGenes$Gene)), length(unique(df_eGenes$SNP))), appendLF=TRUE)
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
