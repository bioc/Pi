#' Function to extract eQTL-gene pairs given a list of SNPs or a customised eQTL mapping data
#'
#' \code{xSNPeqtl} is supposed to extract eQTL-gene pairs given a list of SNPs or a customised eQTL mapping data.
#'
#' @param data NULL or a input vector containing SNPs. If NULL, all SNPs will be considered. If a input vector containing SNPs, SNPs should be provided as dbSNP ID (ie starting with rs). Alternatively, they can be in the format of 'chrN:xxx', where N is either 1-22 or X, xxx is number; for example, 'chr16:28525386'
#' @param include.eQTL genes modulated by eQTL (also Lead SNPs or in LD with Lead SNPs) are also included. By default, it is 'NA' to disable this option. Otherwise, those genes modulated by eQTL will be included. Pre-built eQTL datasets are detailed in the section 'Note'
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
#' 7. eQTLs in CD4 T cells. Sourced from PLoS Genetics 2017, 13(3):e1006643
#' \itemize{
#'  \item{\code{JKpg_CD4}: cis- and trans-eQTLs.}
#'  \item{\code{JKpg_CD4_cis}: cis-eQTLs only.}
#'  \item{\code{JKpg_CD4_trans}: trans-eQTLs only.}
#' }
#' 8. eQTLs in CD8 T cells. Sourced from PLoS Genetics 2017, 13(3):e1006643
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
#' @include xSNPeqtl.r
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
#' df_SGS <- xSNPeqtl(data=AS[,1], include.eQTL="JKscience_TS2A", RData.location=RData.location)
#' }

xSNPeqtl <- function(data=NULL, include.eQTL=c(NA,"JKscience_CD14","JKscience_LPS2","JKscience_LPS24","JKscience_IFN","JKscience_TS2A","JKscience_TS2A_CD14","JKscience_TS2A_LPS2","JKscience_TS2A_LPS24","JKscience_TS2A_IFN","JKscience_TS2B","JKscience_TS2B_CD14","JKscience_TS2B_LPS2","JKscience_TS2B_LPS24","JKscience_TS2B_IFN","JKscience_TS3A","JKng_bcell","JKng_bcell_cis","JKng_bcell_trans","JKng_mono","JKng_mono_cis","JKng_mono_trans","JKpg_CD4","JKpg_CD4_cis","JKpg_CD4_trans","JKpg_CD8","JKpg_CD8_cis","JKpg_CD8_trans","JKnc_neutro","JKnc_neutro_cis","JKnc_neutro_trans","WESTRAng_blood","WESTRAng_blood_cis","WESTRAng_blood_trans","JK_nk","JK_nk_cis","JK_nk_trans", "GTEx_V4_Adipose_Subcutaneous","GTEx_V4_Artery_Aorta","GTEx_V4_Artery_Tibial","GTEx_V4_Esophagus_Mucosa","GTEx_V4_Esophagus_Muscularis","GTEx_V4_Heart_Left_Ventricle","GTEx_V4_Lung","GTEx_V4_Muscle_Skeletal","GTEx_V4_Nerve_Tibial","GTEx_V4_Skin_Sun_Exposed_Lower_leg","GTEx_V4_Stomach","GTEx_V4_Thyroid","GTEx_V4_Whole_Blood","eQTLdb_NK","eQTLdb_CD14","eQTLdb_LPS2","eQTLdb_LPS24","eQTLdb_IFN"), eQTL.customised=NULL, verbose=TRUE, RData.location="http://galahad.well.ox.ac.uk/bigdata")
{

    ######################################################
    # Link to targets based on eQTL
    ######################################################
    
    default.include.eQTL <- c("JKscience_CD14","JKscience_LPS2","JKscience_LPS24","JKscience_IFN","JKscience_TS2A","JKscience_TS2A_CD14","JKscience_TS2A_LPS2","JKscience_TS2A_LPS24","JKscience_TS2A_IFN","JKscience_TS2B","JKscience_TS2B_CD14","JKscience_TS2B_LPS2","JKscience_TS2B_LPS24","JKscience_TS2B_IFN","JKscience_TS3A","JKng_bcell","JKng_bcell_cis","JKng_bcell_trans","JKng_mono","JKng_mono_cis","JKng_mono_trans","JKpg_CD4","JKpg_CD4_cis","JKpg_CD4_trans","JKpg_CD8","JKpg_CD8_cis","JKpg_CD8_trans","JKnc_neutro","JKnc_neutro_cis","JKnc_neutro_trans","WESTRAng_blood","WESTRAng_blood_cis","WESTRAng_blood_trans","JK_nk","JK_nk_cis","JK_nk_trans", "GTEx_V4_Adipose_Subcutaneous","GTEx_V4_Artery_Aorta","GTEx_V4_Artery_Tibial","GTEx_V4_Esophagus_Mucosa","GTEx_V4_Esophagus_Muscularis","GTEx_V4_Heart_Left_Ventricle","GTEx_V4_Lung","GTEx_V4_Muscle_Skeletal","GTEx_V4_Nerve_Tibial","GTEx_V4_Skin_Sun_Exposed_Lower_leg","GTEx_V4_Stomach","GTEx_V4_Thyroid","GTEx_V4_Whole_Blood","eQTLdb_NK","eQTLdb_CD14","eQTLdb_LPS2","eQTLdb_LPS24","eQTLdb_IFN")
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
			
			if(sum(grep("JKscience_TS2A",x,perl=TRUE)) > 0){
				# cis-eQTL
				cis <- xRDataLoader(RData.customised='JKscience_TS2A', RData.location=RData.location, verbose=verbose)
				# either
				if(x=='JKscience_TS2A'){
					minFDR <- apply(cis[,c(9:12)], 1, base::min, na.rm=TRUE)
					df <- data.frame(SNP=cis[,1], Gene=cis[,4], Sig=minFDR, stringsAsFactors=FALSE)
				}else{
					# only
					if(x=='JKscience_TS2A_CD14'){
						j <- 9
					}else if(x=='JKscience_TS2A_LPS2'){
						j <- 10
					}else if(x=='JKscience_TS2A_LPS24'){
						j <- 11
					}else if(x=='JKscience_TS2A_IFN'){
						j <- 12
					}
					ind <- which(!is.na(cis[,j]) & cis[,j]<0.05)
					df <- data.frame(SNP=cis[ind,1], Gene=cis[ind,4], Sig=cis[ind,j], stringsAsFactors=FALSE)
				}
				df <- cbind(df, Context=rep(x,nrow(df)))
			
			}else if(sum(grep("JKscience_TS2B",x,perl=TRUE)) > 0){
				# cis-eQTL
				cis <- xRDataLoader(RData.customised='JKscience_TS2B', RData.location=RData.location, verbose=verbose)
				# either
				if(x=='JKscience_TS2B'){
					minFDR <- apply(cis[,c(9:12)], 1, base::min, na.rm=TRUE)
					df <- data.frame(SNP=cis[,1], Gene=cis[,4], Sig=minFDR, stringsAsFactors=FALSE)
				}else{
					# only
					if(x=='JKscience_TS2B_CD14'){
						j <- 9
					}else if(x=='JKscience_TS2B_LPS2'){
						j <- 10
					}else if(x=='JKscience_TS2B_LPS24'){
						j <- 11
					}else if(x=='JKscience_TS2B_IFN'){
						j <- 12
					}
					ind <- which(!is.na(cis[,j]) & cis[,j]<0.05)
					df <- data.frame(SNP=cis[ind,1], Gene=cis[ind,4], Sig=cis[ind,j], stringsAsFactors=FALSE)
				}
				df <- cbind(df, Context=rep(x,nrow(df)))
			
			}else if(x=='JKscience_TS3A'){
				# trans-eQTL
				trans <- xRDataLoader(RData.customised='JKscience_TS3A', RData.location=RData.location, verbose=verbose)
				minFDR <- apply(trans[,c(9:12)], 1, base::min, na.rm=TRUE)
				df <- data.frame(SNP=trans[,1], Gene=trans[,4], Sig=minFDR, stringsAsFactors=FALSE)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(x=='JKscience_CD14' | x=='JKscience_LPS2' | x=='JKscience_LPS24' | x=='JKscience_IFN'){
				# cis-eQTL
				cis <- xRDataLoader(RData.customised='JKscience_TS2A', RData.location=RData.location, verbose=verbose)
				# trans-eQTL
				trans <- xRDataLoader(RData.customised='JKscience_TS3A', RData.location=RData.location, verbose=verbose)
				## both
				df <- rbind(cis, trans)
				if(x=='JKscience_CD14'){
					j <- 9
				}else if(x=='JKscience_LPS2'){
					j <- 10
				}else if(x=='JKscience_LPS24'){
					j <- 11
				}else if(x=='JKscience_IFN'){
					j <- 12
				}
				ind <- which(!is.na(df[,j]) & df[,j]<0.05)
				df <- data.frame(SNP=df[ind,1], Gene=df[ind,4], Sig=df[ind,j], stringsAsFactors=FALSE)
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(sum(grep("JKng_bcell",x,perl=TRUE)) > 0){
				# b cells
				res_ls <- xRDataLoader(RData.customised='JKng_bcell', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,5], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,5], stringsAsFactors=FALSE)
				if(x=='JKng_bcell'){
					## both
					df <- rbind(df_cis, df_trans)
				}else if(x=='JKng_bcell_cis'){
					df <- df_cis
				}else if(x=='JKng_bcell_trans'){
					df <- df_trans
				}
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(sum(grep("JKng_mono",x,perl=TRUE)) > 0){
				# monocytes
				res_ls <- xRDataLoader(RData.customised='JKng_mono', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,5], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,5], stringsAsFactors=FALSE)
				if(x=='JKng_mono'){
					## both
					df <- rbind(df_cis, df_trans)
				}else if(x=='JKng_mono_cis'){
					df <- df_cis
				}else if(x=='JKng_mono_trans'){
					df <- df_trans
				}
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(sum(grep("JKpg_CD4",x,perl=TRUE)) > 0){
				# CD4
				res_ls <- xRDataLoader(RData.customised='JKpg_CD4', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,6], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,6], stringsAsFactors=FALSE)
				if(x=='JKpg_CD4'){
					## both
					df <- rbind(df_cis, df_trans)
				}else if(x=='JKpg_CD4_cis'){
					df <- df_cis
				}else if(x=='JKpg_CD4_trans'){
					df <- df_trans
				}
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(sum(grep("JKpg_CD8",x,perl=TRUE)) > 0){
				# CD8
				res_ls <- xRDataLoader(RData.customised='JKpg_CD8', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,6], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,6], stringsAsFactors=FALSE)
				if(x=='JKpg_CD8'){
					## both
					df <- rbind(df_cis, df_trans)
				}else if(x=='JKpg_CD8_cis'){
					df <- df_cis
				}else if(x=='JKpg_CD8_trans'){
					df <- df_trans
				}
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(sum(grep("JKnc_neutro",x,perl=TRUE)) > 0){
				# neutrophils
				res_ls <- xRDataLoader(RData.customised='JKnc_neutro', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,6], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,6], stringsAsFactors=FALSE)
				if(x=='JKnc_neutro'){
					## both
					df <- rbind(df_cis, df_trans)
				}else if(x=='JKnc_neutro_cis'){
					df <- df_cis
				}else if(x=='JKnc_neutro_trans'){
					df <- df_trans
				}
				df <- cbind(df, Context=rep(x,nrow(df)))
				
			}else if(sum(grep("WESTRAng_blood",x,perl=TRUE)) > 0){
				# neutrophils
				res_ls <- xRDataLoader(RData.customised='WESTRAng_blood', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,6], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,6], stringsAsFactors=FALSE)
				if(x=='WESTRAng_blood'){
					## both
					df <- rbind(df_cis, df_trans)
				}else if(x=='WESTRAng_blood_cis'){
					df <- df_cis
				}else if(x=='WESTRAng_blood_trans'){
					df <- df_trans
				}
				df <- cbind(df, Context=rep(x,nrow(df)))				
				
			}else if(x=='JK_nk'){
				# NK cells
				res_ls <- xRDataLoader(RData.customised='JK_nk', RData.location=RData.location, verbose=verbose)
				## cis
				df_cis <- data.frame(SNP=res_ls$cis[,1], Gene=res_ls$cis[,2], Sig=res_ls$cis[,6], stringsAsFactors=FALSE)
				## trans
				df_trans <- data.frame(SNP=res_ls$trans[,1], Gene=res_ls$trans[,2], Sig=res_ls$trans[,6], stringsAsFactors=FALSE)
				if(x=='JK_nk'){
					## both
					df <- rbind(df_cis, df_trans)
				}else if(x=='JK_nk_cis'){
					df <- df_cis
				}else if(x=='JK_nk_trans'){
					df <- df_trans
				}
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
	
		## eQTL weight for input SNPs
		ind <- match(df_SGS[,1], data)
		df_SGS <- data.frame(df_SGS[!is.na(ind),])
	
		if(verbose){
			now <- Sys.time()
			message(sprintf("A total of %d input SNPs with %d eGenes", length(data), length(unique(df_SGS[,2]))), appendLF=TRUE)
		}

	}
    
	
    invisible(df_SGS)
}
