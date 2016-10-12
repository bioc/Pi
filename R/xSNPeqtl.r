#' Function to extract eQTL-gene pairs given a list of SNPs or a customised eQTL mapping data
#'
#' \code{xSNPeqtl} is supposed to extract eQTL-gene pairs given a list of SNPs or a customised eQTL mapping data.
#'
#' @param data NULL or a input vector containing SNPs. If NULL, all SNPs will be considered. If a input vector containing SNPs, SNPs should be provided as dbSNP ID (ie starting with rs). Alternatively, they can be in the format of 'chrN:xxx', where N is either 1-22 or X, xxx is number; for example, 'chr16:28525386'
#' @param include.eQTL genes modulated by eQTL (also Lead SNPs or in LD with Lead SNPs) are also included. By default, it is 'NA' to disable this option. Otherwise, those genes modulated by eQTL will be included: immune stimulation in monocytes ('JKscience_TS1A' and 'JKscience_TS2B' for cis-eQTLs or 'JKscience_TS3A' for trans-eQTLs) from Science 2014, 343(6175):1246949; cis- and trans-eQTLs in B cells ('JKng_bcell') and in monocytes ('JKng_mono') from Nature Genetics 2012, 44(5):502-510; cis- and trans-eQTLs in neutrophils ('JKnc_neutro') from Nature Communications 2015, 7(6):7545; cis-eQTLs in NK cells ('JK_nk') which is unpublished. Also supported are GTEx cis-eQTLs from Science 2015, 348(6235):648-60, including 13 tissues: "GTEx_V4_Adipose_Subcutaneous","GTEx_V4_Artery_Aorta","GTEx_V4_Artery_Tibial","GTEx_V4_Esophagus_Mucosa","GTEx_V4_Esophagus_Muscularis","GTEx_V4_Heart_Left_Ventricle","GTEx_V4_Lung","GTEx_V4_Muscle_Skeletal","GTEx_V4_Nerve_Tibial","GTEx_V4_Skin_Sun_Exposed_Lower_leg","GTEx_V4_Stomach","GTEx_V4_Thyroid","GTEx_V4_Whole_Blood".
#' @param eQTL.customised a user-input matrix or data frame with 3 columns: 1st column for SNPs/eQTLs, 2nd column for Genes, and 3rd for eQTL mapping significance level (p-values or FDR). It is designed to allow the user analysing their eQTL data. This customisation (if provided) has the high priority over built-in eQTL data.
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to true for display
#' @param RData.location the characters to tell the location of built-in RData files. See \code{\link{xRDataLoader}} for details
#' @return
#' a data frame with following columns:
#' \itemize{
#'  \item{\code{SNP}: eQTLs}
#'  \item{\code{Gene}: eQTL-containing genes}
#'  \item{\code{Sig}: the eQTL mapping significant level}
#'  \item{\code{Context}: the context in which eQTL data was generated}
#' }
#' @note None
#' @export
#' @seealso \code{\link{xRDataLoader}}
#' @include xSNPeqtl.r
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
#' df_SGS <- xSNPeqtl(data=AS[,1], include.eQTL="JKscience_TS2A")

xSNPeqtl <- function(data=NULL, include.eQTL=c(NA,"JKscience_TS2A","JKscience_TS2B","JKscience_TS3A","JKng_bcell","JKng_mono","JKnc_neutro","JK_nk", "GTEx_V4_Adipose_Subcutaneous","GTEx_V4_Artery_Aorta","GTEx_V4_Artery_Tibial","GTEx_V4_Esophagus_Mucosa","GTEx_V4_Esophagus_Muscularis","GTEx_V4_Heart_Left_Ventricle","GTEx_V4_Lung","GTEx_V4_Muscle_Skeletal","GTEx_V4_Nerve_Tibial","GTEx_V4_Skin_Sun_Exposed_Lower_leg","GTEx_V4_Stomach","GTEx_V4_Thyroid","GTEx_V4_Whole_Blood","eQTLdb_NK","eQTLdb_CD14","eQTLdb_LPS2","eQTLdb_LPS24","eQTLdb_IFN"), eQTL.customised=NULL, verbose=TRUE, RData.location="https://github.com/hfang-bristol/RDataCentre/blob/master/Portal")
{

    ######################################################
    # Link to targets based on eQTL
    ######################################################
    
    default.include.eQTL <- c("JKscience_TS2A","JKscience_TS2B","JKscience_TS3A","JKng_bcell","JKng_mono","JKnc_neutro","JK_nk", "GTEx_V4_Adipose_Subcutaneous","GTEx_V4_Artery_Aorta","GTEx_V4_Artery_Tibial","GTEx_V4_Esophagus_Mucosa","GTEx_V4_Esophagus_Muscularis","GTEx_V4_Heart_Left_Ventricle","GTEx_V4_Lung","GTEx_V4_Muscle_Skeletal","GTEx_V4_Nerve_Tibial","GTEx_V4_Skin_Sun_Exposed_Lower_leg","GTEx_V4_Stomach","GTEx_V4_Thyroid","GTEx_V4_Whole_Blood","eQTLdb_NK","eQTLdb_CD14","eQTLdb_LPS2","eQTLdb_LPS24","eQTLdb_IFN")
	ind <- match(default.include.eQTL, include.eQTL)
	include.eQTL <- default.include.eQTL[!is.na(ind)]
    
    df_SGS <- NULL
    if(length(include.eQTL) > 0 & is.null(eQTL.customised)){
		###########################	
		# built-in eQTL
		###########################	
    	
    	# if GTEx required, only load once
    	if(sum(grep("GTEx_V4_",include.eQTL,perl=TRUE)) > 0){
			GTEx <- xRDataLoader(RData.customised='GTEx_V4', RData.location=RData.location, verbose=verbose)
		}
    
		res_list <- lapply(include.eQTL, function(x){

			if(verbose){
				now <- Sys.time()
				message(sprintf("Processing %s ...", x), appendLF=TRUE)
			}

			if(x=='JKscience_TS2B'){
				# cis-eQTL
				cis <- xRDataLoader(RData.customised='JKscience_TS2B', RData.location=RData.location, verbose=verbose)
				minFDR <- apply(cis[,c(9:12)], 1, min, na.rm=TRUE)
				df <- data.frame(SNP=cis[,1], Gene=cis[,4], Sig=minFDR, stringsAsFactors=FALSE)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(x=='JKscience_TS3A'){
				# trans-eQTL
				trans <- xRDataLoader(RData.customised='JKscience_TS3A', RData.location=RData.location, verbose=verbose)
				minFDR <- apply(trans[,c(9:12)], 1, min, na.rm=TRUE)
				df <- data.frame(SNP=trans[,1], Gene=trans[,4], Sig=minFDR, stringsAsFactors=FALSE)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(x=='JKscience_TS2A'){
				# cis-eQTL
				cis <- xRDataLoader(RData.customised='JKscience_TS2A', RData.location=RData.location, verbose=verbose)
				minFDR <- apply(cis[,c(9:12)], 1, min, na.rm=TRUE)
				df <- data.frame(SNP=cis[,1], Gene=cis[,4], Sig=minFDR, stringsAsFactors=FALSE)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(x=='JKng_bcell'){
				# b cells
				res_ls <- xRDataLoader(RData.customised='JKng_bcell', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,5], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,5], stringsAsFactors=FALSE)
				## both
				df <- rbind(df_cis, df_trans)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(x=='JKng_mono'){
				# monocytes
				res_ls <- xRDataLoader(RData.customised='JKng_mono', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,5], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,5], stringsAsFactors=FALSE)
				## both
				df <- rbind(df_cis, df_trans)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(x=='JKnc_neutro'){
				# neutrophils
				res_ls <- xRDataLoader(RData.customised='JKnc_neutro', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,6], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,6], stringsAsFactors=FALSE)
				## both
				df <- rbind(df_cis, df_trans)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(x=='JK_nk'){
				# NK cells
				cis <- xRDataLoader(RData.customised='JK_nk', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=cis[,1], Gene=cis[,2], Sig=cis[,6], stringsAsFactors=FALSE)
				## both
				df <- df_cis
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(sum(grep("GTEx_V4_",x,perl=TRUE)) > 0){
				y <- gsub("GTEx_V4_","",x)
				cis <- ''
				eval(parse(text=paste("cis <- GTEx$", y, sep="")))
				df <- data.frame(SNP=cis[,1], Gene=cis[,2], Sig=cis[,5], stringsAsFactors=FALSE)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(sum(grep("eQTLdb_",x,perl=TRUE)) > 0){
				cis <- xRDataLoader(RData.customised=x, RData.location=RData.location, verbose=verbose)
				df <- data.frame(SNP=cis[,1], Gene=cis[,2], Sig=cis[,5], stringsAsFactors=FALSE)
				#df <- data.frame(SNP=cis[,1], Gene=cis[,2], Sig=cis[,6], stringsAsFactors=FALSE)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else{
				df <- NULL
			}
			
			return(df)
		})
		## get data frame (SNP Gene FDR)
		SGS <- do.call(rbind, res_list)
	
		############################
		# remove Gene if NA
		# remove SNP if NA
		df_SGS <- SGS[!is.na(SGS[,1]) & !is.na(SGS[,2]),]
		############################
	
		if(verbose){
			now <- Sys.time()
			message(sprintf("%d eGenes are built-in", length(unique(df_SGS[,2]))), appendLF=TRUE)
		}
	
	}else if(!is.null(eQTL.customised)){
		###########################	
		# customised eQTL
		###########################	
		
		if(is.vector(eQTL.customised)){
			# assume a file
			df <- utils::read.delim(file=eQTL.customised, header=FALSE, row.names=NULL, stringsAsFactors=FALSE)
		}else if(is.matrix(eQTL.customised) | is.data.frame(eQTL.customised)){
			df <- eQTL.customised
		}
		
		if(!is.null(df)){
			colnames(df) <- c("SNP", "Gene", "Sig")
			SGS <- cbind(df, Context=rep('Customised',nrow(df)))
			
			############################
			# remove Gene if NA
			# remove SNP if NA
			df_SGS <- SGS[!is.na(SGS[,1]) & !is.na(SGS[,2]),]
			############################
			
			if(verbose){
				now <- Sys.time()
				message(sprintf("%d eGenes are customised", length(unique(df_SGS[,2]))), appendLF=TRUE)
			}
		}
	}
	
	if(!is.null(df_SGS)){
		############################
		# remove Gene if ''
		# remove SNP if ''
		df_SGS <- df_SGS[df_SGS[,1]!='' & df_SGS[,2]!='',]
		############################
	}
	
	###########################################
	if(!is.null(data)){
		## replace '_' with ':'
		data <- gsub("_", ":", data, perl=TRUE)
		## replace 'imm:' with 'chr'
		data <- gsub("imm:", "chr", data, perl=TRUE)
	
		data <- unique(data)
	
		if(verbose){
			now <- Sys.time()
			message(sprintf("A total of %d SNPs are input", length(data)), appendLF=TRUE)
		}
		
		## eQTL weight for input SNPs
		ind <- match(df_SGS[,1], data)
		df_SGS <- data.frame(df_SGS[!is.na(ind),])
	}
    
	
    invisible(df_SGS)
}
