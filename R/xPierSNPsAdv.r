#' Function to prepare genetic predictors given a list of seed SNPs together with the significance level (e.g. GWAS reported p-values)
#'
#' \code{xPierSNPsAdv} is supposed to prepare genetic predictors given a list of seed SNPs together with the significance level (e.g. GWAS reported p-values). Internally it calls \code{\link{xPierSNPs}} to prepare the distance predictor, the eQTL predictors (if required) and the HiC predictors (if required). It returns a list of class "pNode" objects.
#'
#' @param data a named input vector containing the sinificance level for nodes (dbSNP). For this named vector, the element names are dbSNP ID (or in the format such as 'chr16:28525386'), the element values for the significance level (measured as p-value or fdr). Alternatively, it can be a matrix or data frame with two columns: 1st column for dbSNP, 2nd column for the significance level
#' @param include.LD additional SNPs in LD with Lead SNPs are also included. By default, it is 'NA' to disable this option. Otherwise, LD SNPs will be included based on one or more of 5 super-populations from 1000 Genomics Project data (phase 3). They are "AFR", "AMR", "EAS", "EUR", and "SAS". Explanations for population code can be found at \url{http://www.1000genomes.org/faq/which-populations-are-part-your-study}
#' @param LD.customised a user-input matrix or data frame with 3 columns: 1st column for Lead SNPs, 2nd column for LD SNPs, and 3rd for LD r2 value. It is designed to allow the user analysing their pre-calculated LD info. This customisation (if provided) has the high priority over built-in LD SNPs
#' @param LD.r2 the LD r2 value. By default, it is 0.8, meaning that SNPs in LD (r2>=0.8) with input SNPs will be considered as LD SNPs. It can be any value from 0.8 to 1
#' @param significance.threshold the given significance threshold. By default, it is set to NULL, meaning there is no constraint on the significance level when transforming the significance level of SNPs into scores. If given, those SNPs below this are considered significant and thus scored positively. Instead, those above this are considered insigificant and thus receive no score
#' @param score.cap the maximum score being capped. By default, it is set to 10. If NULL, no capping is applied
#' @param distance.max the maximum distance between genes and SNPs. Only those genes no far way from this distance will be considered as seed genes. This parameter will influence the distance-component weights calculated for nearby SNPs per gene
#' @param decay.kernel a character specifying a decay kernel function. It can be one of 'slow' for slow decay, 'linear' for linear decay, and 'rapid' for rapid decay. If no distance weight is used, please select 'constant'
#' @param decay.exponent an integer specifying a decay exponent. By default, it sets to 2
#' @param GR.SNP the genomic regions of SNPs. By default, it is 'dbSNP_GWAS', that is, SNPs from dbSNP (version 146) restricted to GWAS SNPs and their LD SNPs (hg19). It can be 'dbSNP_Common', that is, Common SNPs from dbSNP (version 146) plus GWAS SNPs and their LD SNPs (hg19). Alternatively, the user can specify the customised input. To do so, first save your RData file (containing an GR object) into your local computer, and make sure the GR object content names refer to dbSNP IDs. Then, tell "GR.SNP" with your RData file name (with or without extension), plus specify your file RData path in "RData.location". Note: you can also load your customised GR object directly
#' @param GR.Gene the genomic regions of genes. By default, it is 'UCSC_knownGene', that is, UCSC known genes (together with genomic locations) based on human genome assembly hg19. It can be 'UCSC_knownCanonical', that is, UCSC known canonical genes (together with genomic locations) based on human genome assembly hg19. Alternatively, the user can specify the customised input. To do so, first save your RData file (containing an GR object) into your local computer, and make sure the GR object content names refer to Gene Symbols. Then, tell "GR.Gene" with your RData file name (with or without extension), plus specify your file RData path in "RData.location". Note: you can also load your customised GR object directly
#' @param include.eQTL genes modulated by eQTL (also Lead SNPs or in LD with Lead SNPs) are also included. By default, it is 'NA' to disable this option. Otherwise, those genes modulated by eQTL will be included. Pre-built eQTL datasets are detailed in the section 'Note'
#' @param eQTL.customised a user-input matrix or data frame with 3 columns: 1st column for SNPs/eQTLs, 2nd column for Genes, and 3rd for eQTL mapping significance level (p-values or FDR). It is designed to allow the user analysing their eQTL data. This customisation (if provided) has the high priority over built-in eQTL data
#' @param include.HiC genes linked to input SNPs are also included. By default, it is 'NA' to disable this option. Otherwise, those genes linked to SNPs will be included according to Promoter Capture HiC (PCHiC) datasets. Pre-built HiC datasets are detailed in the section 'Note'
#' @param cdf.function a character specifying a Cumulative Distribution Function (cdf). It can be one of 'exponential' based on exponential cdf, 'empirical' for empirical cdf
#' @param scoring.scheme the method used to calculate seed gene scores under a set of SNPs. It can be one of "sum" for adding up, "max" for the maximum, and "sequential" for the sequential weighting. The sequential weighting is done via: \eqn{\sum_{i=1}{\frac{R_{i}}{i}}}, where \eqn{R_{i}} is the \eqn{i^{th}} rank (in a descreasing order)
#' @param network the built-in network. Currently two sources of network information are supported: the STRING database (version 10) and the Pathway Commons database (version 7). STRING is a meta-integration of undirect interactions from the functional aspect, while Pathways Commons mainly contains both undirect and direct interactions from the physical/pathway aspect. Both have scores to control the confidence of interactions. Therefore, the user can choose the different quality of the interactions. In STRING, "STRING_highest" indicates interactions with highest confidence (confidence scores>=900), "STRING_high" for interactions with high confidence (confidence scores>=700), "STRING_medium" for interactions with medium confidence (confidence scores>=400), and "STRING_low" for interactions with low confidence (confidence scores>=150). For undirect/physical interactions from Pathways Commons, "PCommonsUN_high" indicates undirect interactions with high confidence (supported with the PubMed references plus at least 2 different sources), "PCommonsUN_medium" for undirect interactions with medium confidence (supported with the PubMed references). For direct (pathway-merged) interactions from Pathways Commons, "PCommonsDN_high" indicates direct interactions with high confidence (supported with the PubMed references plus at least 2 different sources), and "PCommonsUN_medium" for direct interactions with medium confidence (supported with the PubMed references). In addition to pooled version of pathways from all data sources, the user can also choose the pathway-merged network from individual sources, that is, "PCommonsDN_Reactome" for those from Reactome, "PCommonsDN_KEGG" for those from KEGG, "PCommonsDN_HumanCyc" for those from HumanCyc, "PCommonsDN_PID" for those froom PID, "PCommonsDN_PANTHER" for those from PANTHER, "PCommonsDN_ReconX" for those from ReconX, "PCommonsDN_TRANSFAC" for those from TRANSFAC, "PCommonsDN_PhosphoSite" for those from PhosphoSite, and "PCommonsDN_CTD" for those from CTD
#' @param weighted logical to indicate whether edge weights should be considered. By default, it sets to false. If true, it only works for the network from the STRING database
#' @param network.customised an object of class "igraph". By default, it is NULL. It is designed to allow the user analysing their customised network data that are not listed in the above argument 'network'. This customisation (if provided) has the high priority over built-in network. If the user provides the "igraph" object with the "weight" edge attribute, RWR will assume to walk on the weighted network
#' @param seeds.inclusive logical to indicate whether non-network seed genes are included for prioritisation. If TRUE (by default), these genes will be added to the netowrk
#' @param normalise the way to normalise the adjacency matrix of the input graph. It can be 'laplacian' for laplacian normalisation, 'row' for row-wise normalisation, 'column' for column-wise normalisation, or 'none'
#' @param restart the restart probability used for Random Walk with Restart (RWR). The restart probability takes the value from 0 to 1, controlling the range from the starting nodes/seeds that the walker will explore. The higher the value, the more likely the walker is to visit the nodes centered on the starting nodes. At the extreme when the restart probability is zero, the walker moves freely to the neighbors at each step without restarting from seeds, i.e., following a random walk (RW)
#' @param normalise.affinity.matrix the way to normalise the output affinity matrix. It can be 'none' for no normalisation, 'quantile' for quantile normalisation to ensure that columns (if multiple) of the output affinity matrix have the same quantiles
#' @param parallel logical to indicate whether parallel computation with multicores is used. By default, it sets to true, but not necessarily does so. Partly because parallel backends available will be system-specific (now only Linux or Mac OS). Also, it will depend on whether these two packages "foreach" and "doMC" have been installed. It can be installed via: \code{source("http://bioconductor.org/biocLite.R"); biocLite(c("foreach","doMC"))}. If not yet installed, this option will be disabled
#' @param multicores an integer to specify how many cores will be registered as the multicore parallel backend to the 'foreach' package. If NULL, it will use a half of cores available in a user's computer. This option only works when parallel computation is enabled
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to true for display
#' @param verbose.details logical to indicate whether the detailed messages from being-called functions will be displayed in the screen. By default, it sets to FALSE enabling messages 
#' @param RData.location the characters to tell the location of built-in RData files. See \code{\link{xRDataLoader}} for details
#' @return
#' A list of class "pNode" objects, each object having a list with following components:
#' \itemize{
#'  \item{\code{priority}: a matrix of nNode X 6 containing node priority information, where nNode is the number of nodes in the input graph, and the 6 columns are "name" (node names), "node" (1 for network genes, 0 for non-network seed genes), "seed" (1 for seeds, 0 for non-seeds), "weight" (weight values),  "priority" (the priority scores that are rescaled to the range [0,1]), "rank" (ranks of the priority scores), "description" (node description)}
#'  \item{\code{g}: an input "igraph" object}
#'  \item{\code{SNP}: a data frame of nSNP X 4 containing input SNPs and/or LD SNPs info, where nSNP is the number of input SNPs and/or LD SNPs, and the 4 columns are "SNP" (dbSNP), "Score" (the SNP score), "Pval" (the SNP p-value), "Flag" (indicative of Lead SNPs or LD SNPs)}
#'  \item{\code{Gene2SNP}: a data frame of nPair X 3 containing Gene-SNP pair info, where nPair is the number of Gene-SNP pairs, and the 3 columns are "Gene" (seed genes), "SNP" (dbSNP), "Score" (an SNP's genetic influential score on a seed gene)}
#'  \item{\code{nGenes}: if not NULL, it is a data frame containing nGene-SNP pair info}
#'  \item{\code{eGenes}: if not NULL, it is a data frame containing eGene-SNP pair info per context}
#'  \item{\code{cGenes}: if not NULL, it is a data frame containing cGene-SNP pair info per context}
#' }
#' @note This function calls \code{\link{xPierSNPs}} in a loop way generating the distance predictor, the eQTL predictors (if required) and the HiC predictors (if required).
#' Pre-built eQTL datasets are described below according to the data sources.\cr
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
#' Pre-built HiC datasets are described below according to the data sources.\cr
#' 1. Promoter Capture HiC datasets in 17 primary blood cell types. Sourced from Cell 2016, 167(5):1369-1384.e19
#' \itemize{
#'  \item{\code{Monocytes}: physical interactions (CHiCAGO score >=5) of promoters (baits) with the other end (preys) in Monocytes.}
#'  \item{\code{Macrophages_M0}: promoter interactomes in Macrophages M0.}
#'  \item{\code{Macrophages_M1}: promoter interactomes in Macrophages M1.}
#'  \item{\code{Macrophages_M2}: promoter interactomes in Macrophages M2.}
#'  \item{\code{Neutrophils}: promoter interactomes in Neutrophils.}
#'  \item{\code{Megakaryocytes}: promoter interactomes in Megakaryocytes.}
#'  \item{\code{Endothelial_precursors}: promoter interactomes in Endothelial precursors.}
#'  \item{\code{Fetal_thymus}: promoter interactomes in Fetal thymus.}
#'  \item{\code{Naive_CD4_T_cells}: promoter interactomes in Naive CD4+ T cells.}
#'  \item{\code{Total_CD4_T_cells}: promoter interactomes in Total CD4+ T cells.}
#'  \item{\code{Activated_total_CD4_T_cells}: promoter interactomes in Activated total CD4+ T cells.}
#'  \item{\code{Nonactivated_total_CD4_T_cells}: promoter interactomes in Nonactivated total CD4+ T cells.}
#'  \item{\code{Naive_CD8_T_cells}: promoter interactomes in Naive CD8+ T cells.}
#'  \item{\code{Total_CD8_T_cells}: promoter interactomes in Total CD8+ T cells.}
#'  \item{\code{Naive_B_cells}: promoter interactomes in Naive B cells.}
#'  \item{\code{Total_B_cells}: promoter interactomes in Total B cells.}
#' }
#' 2. Promoter Capture HiC datasets (involving active promoters and enhancers) in 9 primary blood cell types. Sourced from Cell 2016, 167(5):1369-1384.e19
#' \itemize{
#'  \item{\code{PE.Monocytes}: physical interactions (CHiCAGO score >=5) of promoters (baits) with the other end (enhancers as preys) in Monocytes.}
#'  \item{\code{PE.Macrophages_M0}: promoter-enhancer interactomes in Macrophages M0.}
#'  \item{\code{PE.Macrophages_M1}: promoter-enhancer interactomes in Macrophages M1.}
#'  \item{\code{PE.Macrophages_M2}: promoter-enhancer interactomes in Macrophages M2.}
#'  \item{\code{PE.Neutrophils}: promoter-enhancer interactomes in Neutrophils.}
#'  \item{\code{PE.Megakaryocytes}: promoter-enhancer interactomes in Megakaryocytes.}
#'  \item{\code{PE.Erythroblasts}: promoter-enhancer interactomes in Erythroblasts.}
#'  \item{\code{PE.Naive_CD4_T_cells}: promoter-enhancer interactomes in Naive CD4+ T cells.}
#'  \item{\code{PE.Naive_CD8_T_cells}: promoter-enhancer interactomes in Naive CD8+ T cells.}
#' }
#' @export
#' @seealso \code{\link{xPierSNPs}}, \code{\link{xPierMatrix}}
#' @include xPierSNPsAdv.r
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
#' ls_pNode <- xPierSNPsAdv(data=AS, include.eQTL="JKng_mono", include.HiC='Monocytes', network="PCommonsUN_medium", restart=0.7, RData.location=RData.location)
#' }

xPierSNPsAdv <- function(data, include.LD=NA, LD.customised=NULL, LD.r2=0.8, significance.threshold=5e-5, score.cap=10, distance.max=2000, decay.kernel=c("slow","constant","linear","rapid"), decay.exponent=2, GR.SNP=c("dbSNP_GWAS","dbSNP_Common"), GR.Gene=c("UCSC_knownGene","UCSC_knownCanonical"), include.eQTL=c(NA,"JKscience_CD14","JKscience_LPS2","JKscience_LPS24","JKscience_IFN","JKscience_TS2A","JKscience_TS2A_CD14","JKscience_TS2A_LPS2","JKscience_TS2A_LPS24","JKscience_TS2A_IFN","JKscience_TS2B","JKscience_TS2B_CD14","JKscience_TS2B_LPS2","JKscience_TS2B_LPS24","JKscience_TS2B_IFN","JKscience_TS3A","JKng_bcell","JKng_bcell_cis","JKng_bcell_trans","JKng_mono","JKng_mono_cis","JKng_mono_trans","JKpg_CD4","JKpg_CD4_cis","JKpg_CD4_trans","JKpg_CD8","JKpg_CD8_cis","JKpg_CD8_trans","JKnc_neutro","JKnc_neutro_cis","JKnc_neutro_trans","WESTRAng_blood","WESTRAng_blood_cis","WESTRAng_blood_trans","JK_nk","JK_nk_cis","JK_nk_trans", "GTEx_V4_Adipose_Subcutaneous","GTEx_V4_Artery_Aorta","GTEx_V4_Artery_Tibial","GTEx_V4_Esophagus_Mucosa","GTEx_V4_Esophagus_Muscularis","GTEx_V4_Heart_Left_Ventricle","GTEx_V4_Lung","GTEx_V4_Muscle_Skeletal","GTEx_V4_Nerve_Tibial","GTEx_V4_Skin_Sun_Exposed_Lower_leg","GTEx_V4_Stomach","GTEx_V4_Thyroid","GTEx_V4_Whole_Blood","eQTLdb_NK","eQTLdb_CD14","eQTLdb_LPS2","eQTLdb_LPS24","eQTLdb_IFN"), eQTL.customised=NULL, include.HiC=c(NA, "Monocytes","Macrophages_M0","Macrophages_M1","Macrophages_M2","Neutrophils","Megakaryocytes","Endothelial_precursors","Erythroblasts","Fetal_thymus","Naive_CD4_T_cells","Total_CD4_T_cells","Activated_total_CD4_T_cells","Nonactivated_total_CD4_T_cells","Naive_CD8_T_cells","Total_CD8_T_cells","Naive_B_cells","Total_B_cells","PE.Monocytes","PE.Macrophages_M0","PE.Macrophages_M1","PE.Macrophages_M2","PE.Neutrophils","PE.Megakaryocytes","PE.Erythroblasts","PE.Naive_CD4_T_cells","PE.Naive_CD8_T_cells"), cdf.function=c("empirical","exponential"), scoring.scheme=c("max","sum","sequential"), network=c("STRING_highest","STRING_high","STRING_medium","STRING_low","PCommonsUN_high","PCommonsUN_medium","PCommonsDN_high","PCommonsDN_medium","PCommonsDN_Reactome","PCommonsDN_KEGG","PCommonsDN_HumanCyc","PCommonsDN_PID","PCommonsDN_PANTHER","PCommonsDN_ReconX","PCommonsDN_TRANSFAC","PCommonsDN_PhosphoSite","PCommonsDN_CTD"), weighted=FALSE, network.customised=NULL, seeds.inclusive=TRUE, normalise=c("laplacian","row","column","none"), restart=0.7, normalise.affinity.matrix=c("none","quantile"), parallel=TRUE, multicores=NULL, verbose=TRUE, verbose.details=FALSE, RData.location="http://galahad.well.ox.ac.uk/bigdata")
{

    startT <- Sys.time()
    if(verbose){
        message(paste(c("Start at ",as.character(startT)), collapse=""), appendLF=TRUE)
        message("", appendLF=TRUE)
    }
    ####################################################################################
    
    ## match.arg matches arg against a table of candidate values as specified by choices, where NULL means to take the first one
    decay.kernel <- match.arg(decay.kernel)
    cdf.function <- match.arg(cdf.function)
    scoring.scheme <- match.arg(scoring.scheme)
    network <- match.arg(network)
    normalise <- match.arg(normalise)
    normalise.affinity.matrix <- match.arg(normalise.affinity.matrix)
    
    ## force verbose.details to be FALSE if verbose is FALSE
    if(verbose==FALSE){
    	verbose.details <- FALSE
    }
    ####################################################################################
	if(verbose){
		now <- Sys.time()
		message(sprintf("Preparing the distance predictor (%s) ...", as.character(now)), appendLF=TRUE)
	}
	relative.importance <- c(1,0,0)
    pNode_distance <- xPierSNPs(data=data, include.LD=include.LD, LD.customised=LD.customised, LD.r2=LD.r2, significance.threshold=significance.threshold, score.cap=score.cap, distance.max=distance.max, decay.kernel=decay.kernel, decay.exponent=decay.exponent, GR.SNP=GR.SNP, GR.Gene=GR.Gene, include.eQTL=NA, eQTL.customised=NULL, include.HiC=NA, cdf.function=cdf.function, relative.importance=relative.importance, scoring.scheme=scoring.scheme, network=network, weighted=weighted, network.customised=network.customised, seeds.inclusive=seeds.inclusive, normalise=normalise, restart=restart, normalise.affinity.matrix=normalise.affinity.matrix, parallel=parallel, multicores=multicores, verbose=verbose.details, RData.location=RData.location)
    ls_pNode_distance <- list(pNode_distance)
    names(ls_pNode_distance) <- paste('nearbyGenes_', distance.max, '_', decay.kernel, sep='')
    
    ####################################################################################
    
    include.eQTLs <- include.eQTL[!is.na(include.eQTL)]
    if(length(include.eQTLs)>0){
		names(include.eQTLs) <- include.eQTLs
		ls_pNode_eQTL <- lapply(include.eQTLs, function(x){
			if(verbose){
				now <- Sys.time()
				message(sprintf("Preparing the eQTL predictor '%s' (%s) ...", x, as.character(now)), appendLF=TRUE)
			}
			relative.importance <- c(0,1,0)
			pNode <- xPierSNPs(data=data, include.LD=include.LD, LD.customised=LD.customised, LD.r2=LD.r2, significance.threshold=significance.threshold, score.cap=score.cap, distance.max=distance.max, decay.kernel=decay.kernel, decay.exponent=decay.exponent, GR.SNP=GR.SNP, GR.Gene=GR.Gene, include.eQTL=x, eQTL.customised=NULL, include.HiC=NA, cdf.function=cdf.function, relative.importance=relative.importance, scoring.scheme=scoring.scheme, network=network, weighted=weighted, network.customised=network.customised, seeds.inclusive=seeds.inclusive, normalise=normalise, restart=restart, normalise.affinity.matrix=normalise.affinity.matrix, parallel=parallel, multicores=multicores, verbose=verbose.details, RData.location=RData.location)
			if(verbose & is.null(pNode)){
				message(sprintf("\tNote: this predictor '%s' has NULL", x), appendLF=TRUE)
			}
			return(pNode)
		})
		names(ls_pNode_eQTL) <- paste('eQTL_', names(ls_pNode_eQTL), sep='')
    }else{
    	ls_pNode_eQTL <- NULL
    }
    
    include.HiCs <- include.HiC[!is.na(include.HiC)]
    if(length(include.HiCs)>0){
		names(include.HiCs) <- include.HiCs
		ls_pNode_HiC <- lapply(include.HiCs, function(x){
			if(verbose){
				now <- Sys.time()
				message(sprintf("Preparing the HiC predictor '%s' (%s) ...", x, as.character(now)), appendLF=TRUE)
			}
			relative.importance <- c(0,0,1)
			pNode <- xPierSNPs(data=data, include.LD=include.LD, LD.customised=LD.customised, LD.r2=LD.r2, significance.threshold=significance.threshold, score.cap=score.cap, distance.max=distance.max, decay.kernel=decay.kernel, decay.exponent=decay.exponent, GR.SNP=GR.SNP, GR.Gene=GR.Gene, include.eQTL=NA, eQTL.customised=NULL, include.HiC=x, cdf.function=cdf.function, relative.importance=relative.importance, scoring.scheme=scoring.scheme, network=network, weighted=weighted, network.customised=network.customised, seeds.inclusive=seeds.inclusive, normalise=normalise, restart=restart, normalise.affinity.matrix=normalise.affinity.matrix, parallel=parallel, multicores=multicores, verbose=verbose.details, RData.location=RData.location)
			if(verbose & is.null(pNode)){
				message(sprintf("\tNote: this predictor '%s' has NULL", x), appendLF=TRUE)
			}
			return(pNode)
		})
		names(ls_pNode_HiC) <- paste('HiC_', names(ls_pNode_HiC), sep='')
	}else{
		ls_pNode_HiC <- NULL
	}
    
    ##########################################################################################
    ## prioritisation equally
    #relative.importance <- c(1/3,1/3,1/3)
    #pNode_all <- xPierSNPs(data=data, include.LD=include.LD, LD.customised=LD.customised, LD.r2=LD.r2, significance.threshold=significance.threshold, score.cap=score.cap, distance.max=distance.max, decay.kernel=decay.kernel, decay.exponent=decay.exponent, GR.SNP=GR.SNP, GR.Gene=GR.Gene, include.eQTL=include.eQTLs, eQTL.customised=NULL, include.HiC=include.HiCs, cdf.function=cdf.function, relative.importance=relative.importance, scoring.scheme=scoring.scheme, network=network, weighted=weighted, network.customised=network.customised, seeds.inclusive=seeds.inclusive, normalise=normalise, restart=restart, normalise.affinity.matrix=normalise.affinity.matrix, parallel=parallel, multicores=multicores, verbose=verbose, RData.location=RData.location)
    ##########################################################################################
    ls_pNode <- c(ls_pNode_distance, ls_pNode_eQTL, ls_pNode_HiC)
    
    ####################################################################################
    endT <- Sys.time()
    if(verbose){
        message(paste(c("\nFinish at ",as.character(endT)), collapse=""), appendLF=TRUE)
    }
    
    runTime <- as.numeric(difftime(strptime(endT, "%Y-%m-%d %H:%M:%S"), strptime(startT, "%Y-%m-%d %H:%M:%S"), units="secs"))
    message(paste(c("Runtime in total is: ",runTime," secs\n"), collapse=""), appendLF=TRUE)
    
    invisible(ls_pNode)
}
