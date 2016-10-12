#' Function to priorise genes given a list of seed SNPs together with the significance level (e.g. GWAS reported p-values)
#'
#' \code{xPierSNPs} is supposed to priorise genes given a list of seed SNPs together with the significance level. To priorise genes, it first defines and scores seed genes: nearby genes and eQTL genes. With seed genes and their scores, it then uses Random Walk with Restart (RWR) to calculate the affinity score of all nodes in the input graph to the seed genes. The priority score is the affinity score. Parallel computing is also supported for Linux or Mac operating systems. It returns an object of class "pNode".
#'
#' @param data a named input vector containing the sinificance level for nodes (dbSNP). For this named vector, the element names are dbSNP ID (or in the format such as 'chr16:28525386'), the element values for the significance level (measured as p-value or fdr). Alternatively, it can be a matrix or data frame with two columns: 1st column for dbSNP, 2nd column for the significance level
#' @param include.LD additional SNPs in LD with Lead SNPs are also included. By default, it is 'NA' to disable this option. Otherwise, LD SNPs will be included based on one or more of 26 populations and 5 super populations from 1000 Genomics Project data (phase 3). The population can be one of 5 super populations ("AFR", "AMR", "EAS", "EUR", "SAS"), or one of 26 populations ("ACB", "ASW", "BEB", "CDX", "CEU", "CHB", "CHS", "CLM", "ESN", "FIN", "GBR", "GIH", "GWD", "IBS", "ITU", "JPT", "KHV", "LWK", "MSL", "MXL", "PEL", "PJL", "PUR", "STU", "TSI", "YRI"). Explanations for population code can be found at \url{http://www.1000genomes.org/faq/which-populations-are-part-your-study}
#' @param LD.customised a user-input matrix or data frame with 3 columns: 1st column for Lead SNPs, 2nd column for LD SNPs, and 3rd for LD r2 value. It is designed to allow the user analysing their pre-calculated LD info. This customisation (if provided) has the high priority over built-in LD SNPs
#' @param LD.r2 the LD r2 value. By default, it is 0.8, meaning that SNPs in LD (r2>=0.8) with input SNPs will be considered as LD SNPs. It can be any value from 0.8 to 1
#' @param significance.threshold the given significance threshold. By default, it is set to NULL, meaning there is no constraint on the significance level when transforming the significance level of SNPs into scores. If given, those SNPs below this are considered significant and thus scored positively. Instead, those above this are considered insigificant and thus receive no score
#' @param distance.max the maximum distance between genes and SNPs. Only those genes no far way from this distance will be considered as seed genes. This parameter will influence the distance-component weights calculated for nearby SNPs per gene
#' @param decay.kernel a character specifying a decay kernel function. It can be one of 'slow' for slow decay, 'linear' for linear decay, and 'rapid' for rapid decay
#' @param decay.exponent an integer specifying a decay exponent. By default, it sets to 2
#' @param GR.SNP the genomic regions of SNPs. By default, it is 'dbSNP_GWAS', that is, SNPs from dbSNP (version 146) restricted to GWAS SNPs and their LD SNPs (hg19). It can be 'dbSNP_Common', that is, Common SNPs from dbSNP (version 146) plus GWAS SNPs and their LD SNPs (hg19). Alternatively, the user can specify the customised input. To do so, first save your RData file (containing an GR object) into your local computer, and make sure the GR object content names refer to dbSNP IDs. Then, tell "GR.SNP" with your RData file name (with or without extension), plus specify your file RData path in "RData.location". Note: you can also load your customised GR object directly
#' @param GR.Gene the genomic regions of genes. By default, it is 'UCSC_knownGene', that is, UCSC known genes (together with genomic locations) based on human genome assembly hg19. It can be 'UCSC_knownCanonical', that is, UCSC known canonical genes (together with genomic locations) based on human genome assembly hg19. Alternatively, the user can specify the customised input. To do so, first save your RData file (containing an GR object) into your local computer, and make sure the GR object content names refer to Gene Symbols. Then, tell "GR.Gene" with your RData file name (with or without extension), plus specify your file RData path in "RData.location". Note: you can also load your customised GR object directly
#' @param include.eQTL genes modulated by eQTL (also Lead SNPs or in LD with Lead SNPs) are also included. By default, it is 'NA' to disable this option. Otherwise, those genes modulated by eQTL will be included: immune stimulation in monocytes ('JKscience_TS1A' and 'JKscience_TS2B' for cis-eQTLs or 'JKscience_TS3A' for trans-eQTLs) from Science 2014, 343(6175):1246949; cis- and trans-eQTLs in B cells ('JKng_bcell') and in monocytes ('JKng_mono') from Nature Genetics 2012, 44(5):502-510; cis- and trans-eQTLs in neutrophils ('JKnc_neutro') from Nature Communications 2015, 7(6):7545; cis-eQTLs in NK cells ('JK_nk') which is unpublished. Also supported are GTEx cis-eQTLs from Science 2015, 348(6235):648-60, including 13 tissues: 'GTEx_Adipose_Subcutaneous','GTEx_Artery_Aorta','GTEx_Artery_Tibial','GTEx_Esophagus_Mucosa','GTEx_Esophagus_Muscularis','GTEx_Heart_Left_Ventricle','GTEx_Lung','GTEx_Muscle_Skeletal','GTEx_Nerve_Tibial','GTEx_Skin_Sun_Exposed_Lower_leg','GTEx_Stomach','GTEx_Thyroid','GTEx_Whole_Blood'.
#' @param eQTL.customised a user-input matrix or data frame with 3 columns: 1st column for SNPs/eQTLs, 2nd column for Genes, and 3rd for eQTL mapping significance level (p-values or FDR). It is designed to allow the user analysing their eQTL data. This customisation (if provided) has the high priority over built-in eQTL data.
#' @param cdf.function a character specifying a Cumulative Distribution Function (cdf). It can be one of 'exponential' based on exponential cdf, 'empirical' for empirical cdf
#' @param relative.importance a vector specifying the relative importance of the distance weight and eQTL weight
#' @param scoring.scheme the method used to calculate seed gene scores under a set of SNPs. It can be one of "sum" for adding up, "max" for the maximum, and "sequential" for the sequential weighting. The sequential weighting is done via: \eqn{\sum_{i=1}{\frac{R_{i}}{i}}}, where \eqn{R_{i}} is the \eqn{i^{th}} rank (in a descreasing order)
#' @param network the built-in network. Currently two sources of network information are supported: the STRING database (version 10) and the Pathways Commons database (version 7). STRING is a meta-integration of undirect interactions from the functional aspect, while Pathways Commons mainly contains both undirect and direct interactions from the physical/pathway aspect. Both have scores to control the confidence of interactions. Therefore, the user can choose the different quality of the interactions. In STRING, "STRING_highest" indicates interactions with highest confidence (confidence scores>=900), "STRING_high" for interactions with high confidence (confidence scores>=700), "STRING_medium" for interactions with medium confidence (confidence scores>=400), and "STRING_low" for interactions with low confidence (confidence scores>=150). For undirect/physical interactions from Pathways Commons, "PCommonsUN_high" indicates undirect interactions with high confidence (supported with the PubMed references plus at least 2 different sources), "PCommonsUN_medium" for undirect interactions with medium confidence (supported with the PubMed references). For direct (pathway-merged) interactions from Pathways Commons, "PCommonsDN_high" indicates direct interactions with high confidence (supported with the PubMed references plus at least 2 different sources), and "PCommonsUN_medium" for direct interactions with medium confidence (supported with the PubMed references). In addtion to pooled version of pathways from all data sources, the user can also choose the pathway-merged network from individual sources, that is, "PCommonsDN_Reactome" for those from Reactome, "PCommonsDN_KEGG" for those from KEGG, "PCommonsDN_HumanCyc" for those from HumanCyc, "PCommonsDN_PID" for those froom PID, "PCommonsDN_PANTHER" for those from PANTHER, "PCommonsDN_ReconX" for those from ReconX, "PCommonsDN_TRANSFAC" for those from TRANSFAC, "PCommonsDN_PhosphoSite" for those from PhosphoSite, and "PCommonsDN_CTD" for those from CTD
#' @param weighted logical to indicate whether edge weights should be considered. By default, it sets to false. If true, it only works for the network from the STRING database
#' @param network.customised an object of class "igraph". By default, it is NULL. It is designed to allow the user analysing their customised network data that are not listed in the above argument 'network'. This customisation (if provided) has the high priority over built-in network. If the user provides the "igraph" object with the "weight" edge attribute, RWR will assume to walk on the weighted network
#' @param normalise the way to normalise the adjacency matrix of the input graph. It can be 'laplacian' for laplacian normalisation, 'row' for row-wise normalisation, 'column' for column-wise normalisation, or 'none'
#' @param restart the restart probability used for Random Walk with Restart (RWR). The restart probability takes the value from 0 to 1, controlling the range from the starting nodes/seeds that the walker will explore. The higher the value, the more likely the walker is to visit the nodes centered on the starting nodes. At the extreme when the restart probability is zero, the walker moves freely to the neighbors at each step without restarting from seeds, i.e., following a random walk (RW)
#' @param normalise.affinity.matrix the way to normalise the output affinity matrix. It can be 'none' for no normalisation, 'quantile' for quantile normalisation to ensure that columns (if multiple) of the output affinity matrix have the same quantiles
#' @param parallel logical to indicate whether parallel computation with multicores is used. By default, it sets to true, but not necessarily does so. Partly because parallel backends available will be system-specific (now only Linux or Mac OS). Also, it will depend on whether these two packages "foreach" and "doMC" have been installed. It can be installed via: \code{source("http://bioconductor.org/biocLite.R"); biocLite(c("foreach","doMC"))}. If not yet installed, this option will be disabled
#' @param multicores an integer to specify how many cores will be registered as the multicore parallel backend to the 'foreach' package. If NULL, it will use a half of cores available in a user's computer. This option only works when parallel computation is enabled
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to true for display
#' @param RData.location the characters to tell the location of built-in RData files. See \code{\link{xRDataLoader}} for details
#' @return
#' an object of class "pNode", a list with following components:
#' \itemize{
#'  \item{\code{priority}: a matrix of nNode X 4 containing node priority information, where nNode is the number of nodes in the input graph, and the 4 columns are "name" (node names), "seed" (1 for seeds, 0 for non-seeds), "weight" (weight/score values for seed genes),  "priority" (the priority scores that are rescaled to the range [0,1]), "rank" (ranks of the priority scores)}
#'  \item{\code{g}: an input "igraph" object}
#'  \item{\code{SNP}: a data frame of nSNP X 3 containing input SNPs and/or LD SNPs info, where nSNP is the number of input SNPs and/or LD SNPs, and the 4 columns are "SNP" (dbSNP), "Score" (the SNP score), "Pval" (the SNP p-value), "Flag" (indicative of Lead SNPs or LD SNPs)}
#'  \item{\code{Gene2SNP}: a data frame of nPair X 3 containing Gene-SNP pair info, where nPair is the number of Gene-SNP pairs, and the 3 columns are "Gene" (seed genes), "SNP" (dbSNP), "Score" (an SNP's genetic influential score on a seed gene), "Pval" (the SNP p-value)}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note The search procedure is heuristic to find the subgraph with the maximum score:
#' \itemize{
#' \item{i) \code{\link{xSNPscores}} used to calculate the SNP score.}
#' \item{ii) \code{\link{xSNP2nGenes}} used to define and score the nearby genes.}
#' \item{iii) \code{\link{xSNP2eGenes}} used to define and score the eQTL genes.}
#' \item{iv) define seed genes as the nearby genes in ii) and the eQTL genes in iii), which are then scored in an integrative manner.}
#' \item{v) \code{\link{xPierGenes}} used to prioritise genes using an input graph and a list of seed genes and their scores from iv). The priority score is the affinity score estimated by Random Walk with Restart (RWR), measured as the affinity of all nodes in the graph to the seeds.}
#' }
#' @export
#' @seealso \code{\link{xSNPscores}}, \code{\link{xSNP2nGenes}}, \code{\link{xSNP2eGenes}}, \code{\link{xSparseMatrix}}, \code{\link{xSM2DF}}, \code{\link{xPier}}, \code{\link{xPierGenes}}, \code{\link{xPierPathways}}
#' @include xPierSNPs.r
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
#' \dontrun{
#' # b) perform priority analysis
#' pNode <- xPierSNPs(data=AS, include.LD=NA, include.eQTL=c("JKscience_TS2A","JKscience_TS3A"), network="PCommonsUN_medium", restart=0.7)
#'
#' # c) save to the file called 'SNPs_priority.txt'
#' write.table(pNode$priority, file="SNPs_priority.txt", sep="\t", row.names=FALSE)
#' 
#' # d) manhattan plot
#' mp <- xPierManhattan(pNode, highlight.top=10)
#' #pdf(file="Gene_manhattan.pdf", height=6, width=12, compress=TRUE)
#' print(mp)
#' #dev.off()
#' }

xPierSNPs <- function(data, include.LD=NA, LD.customised=NULL, LD.r2=0.8, significance.threshold=5e-5, distance.max=200000, decay.kernel=c("rapid","slow","linear"), decay.exponent=2, GR.SNP=c("dbSNP_GWAS","dbSNP_Common"), GR.Gene=c("UCSC_knownGene","UCSC_knownCanonical"), include.eQTL=c(NA,"JKscience_TS2A","JKscience_TS2B","JKscience_TS3A","JKng_bcell","JKng_mono","JKnc_neutro","JK_nk", "GTEx_V4_Adipose_Subcutaneous","GTEx_V4_Artery_Aorta","GTEx_V4_Artery_Tibial","GTEx_V4_Esophagus_Mucosa","GTEx_V4_Esophagus_Muscularis","GTEx_V4_Heart_Left_Ventricle","GTEx_V4_Lung","GTEx_V4_Muscle_Skeletal","GTEx_V4_Nerve_Tibial","GTEx_V4_Skin_Sun_Exposed_Lower_leg","GTEx_V4_Stomach","GTEx_V4_Thyroid","GTEx_V4_Whole_Blood","eQTLdb_NK","eQTLdb_CD14","eQTLdb_LPS2","eQTLdb_LPS24","eQTLdb_IFN"), eQTL.customised=NULL, cdf.function=c("empirical","exponential"), relative.importance=c(0.5, 0.5), scoring.scheme=c("max","sum","sequential"), network=c("STRING_highest","STRING_high","STRING_medium","STRING_low","PCommonsUN_high","PCommonsUN_medium","PCommonsDN_high","PCommonsDN_medium","PCommonsDN_Reactome","PCommonsDN_KEGG","PCommonsDN_HumanCyc","PCommonsDN_PID","PCommonsDN_PANTHER","PCommonsDN_ReconX","PCommonsDN_TRANSFAC","PCommonsDN_PhosphoSite","PCommonsDN_CTD"), weighted=FALSE, network.customised=NULL, normalise=c("laplacian","row","column","none"), restart=0.75, normalise.affinity.matrix=c("none","quantile"), parallel=TRUE, multicores=NULL, verbose=TRUE, RData.location="https://github.com/hfang-bristol/RDataCentre/blob/master/Portal")
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
    
    ####################################################################################
    
    if(verbose){
        now <- Sys.time()
        message(sprintf("\n#######################################################", appendLF=TRUE))
        message(sprintf("'xSNPscores' is being called to score SNPs (%s):", as.character(now)), appendLF=TRUE)
        message(sprintf("#######################################################", appendLF=TRUE))
    }
    
	df_SNP <- xSNPscores(data=data, include.LD=include.LD, LD.customised=LD.customised, LD.r2=LD.r2, significance.threshold=significance.threshold, verbose=verbose, RData.location=RData.location)
	
	if(verbose){
        now <- Sys.time()
        message(sprintf("#######################################################", appendLF=TRUE))
        message(sprintf("'xSNPscores' has been finished (%s)!", as.character(now)), appendLF=TRUE)
        message(sprintf("#######################################################\n", appendLF=TRUE))
    }
    
    ####################################################################################
    
    if(verbose){
        now <- Sys.time()
        message(sprintf("\n#######################################################", appendLF=TRUE))
        message(sprintf("'xSNP2nGenes' is being called to define nearby genes (%s):", as.character(now)), appendLF=TRUE)
        message(sprintf("#######################################################", appendLF=TRUE))
    }
    
    
    if(relative.importance[1] != 0){
		df_nGenes <- xSNP2nGenes(data=df_SNP$SNP, distance.max=distance.max, decay.kernel=decay.kernel, decay.exponent=decay.exponent, GR.SNP=GR.SNP, GR.Gene=GR.Gene, verbose=verbose, RData.location=RData.location)
	}else{
		df_nGenes <- NULL
		if(verbose){
			now <- Sys.time()
			message(sprintf("No nearby genes are defined"), appendLF=TRUE)
		}
	}
		
	if(verbose){
        now <- Sys.time()
        message(sprintf("#######################################################", appendLF=TRUE))
        message(sprintf("'xSNP2nGenes' has been finished (%s)!", as.character(now)), appendLF=TRUE)
        message(sprintf("#######################################################\n", appendLF=TRUE))
    }
    
    ####################################################################################
    
    if(verbose){
        now <- Sys.time()
        message(sprintf("\n#######################################################", appendLF=TRUE))
        message(sprintf("'xSNP2eGenes' is being called to define eQTL-containing genes (%s):", as.character(now)), appendLF=TRUE)
        message(sprintf("#######################################################", appendLF=TRUE))
    }
    
	df_eGenes <- xSNP2eGenes(data=df_SNP$SNP, include.eQTL=include.eQTL, eQTL.customised=eQTL.customised, cdf.function=cdf.function, plot=FALSE, verbose=verbose, RData.location=RData.location)
	
	if(verbose){
        now <- Sys.time()
        message(sprintf("#######################################################", appendLF=TRUE))
        message(sprintf("'xSNP2eGenes' has been finished (%s)!", as.character(now)), appendLF=TRUE)
        message(sprintf("#######################################################\n", appendLF=TRUE))
    }
    ####################################################################################
    
    if(is.null(df_nGenes) & is.null(df_eGenes)){
    	G2S <- NULL
    }else{
    
		## df_SNP df_nGenes df_eGenes
		allGenes <- sort(union(df_nGenes$Gene, df_eGenes$Gene))
		allSNPs <- sort(df_SNP$SNP)
	
		## sparse matrix of nGenes X SNPs
		G2S_n <- xSparseMatrix(df_nGenes[,c("Gene","SNP","Weight")], rows=allGenes, columns=allSNPs, verbose=FALSE)
		## sparse matrix of eGenes X SNPs
		G2S_e <- xSparseMatrix(df_eGenes[,c("Gene","SNP","Weight")], rows=allGenes, columns=allSNPs, verbose=FALSE)
	
		## combine both sparse matrix
		if(is.null(G2S_e)){
			G2S <- G2S_n
		}else if(is.null(G2S_n)){
			G2S <- G2S_e
		}else if(!is.null(G2S_n) & !is.null(G2S_e)){
			G2S <- G2S_n * relative.importance[1] + G2S_e * relative.importance[2]
		}
    
    }
    
    #######################
    ## if NULL, return NULL
    if(is.null(G2S)){
    	return(NULL)
    }
    #######################
        
    ## consider SNP scores
    ind <- match(df_SNP$SNP, colnames(G2S))
    ########
    df_SNP <- df_SNP[ind,]
    ########
    SNP_score <- df_SNP$Score
    names(SNP_score) <- colnames(G2S)
    ## convert into matrix
    mat_SNP_score <- matrix(rep(SNP_score,each=nrow(G2S)), nrow=nrow(G2S))
    
    ## calculate genetic influence score for a gene-SNP pair
    G2S_score <- G2S * mat_SNP_score
    
    ## calculate genetic influence score under a set of SNPs for each seed gene
    if(scoring.scheme=='max'){
    	seeds.genes <- apply(G2S_score, 1, function(x) {
            base::max(x)
        })
    }else if(scoring.scheme=='sum'){
    	seeds.genes <- apply(G2S_score, 1, function(x) {
            base::sum(x)
        })
    }else if(scoring.scheme=='sequential'){
        seeds.genes <- apply(G2S_score, 1, function(x) {
        	base::sum(base::sort(x, decreasing=TRUE) / (1:length(x)))
        })
    }
	
	if(verbose){
		now <- Sys.time()
		message(sprintf("%d Genes are defined as seeds and scored using '%s' scoring scheme from %d SNPs", length(seeds.genes), scoring.scheme, ncol(G2S_score)), appendLF=TRUE)
	}
    
    ######################################################################################
    
    if(verbose){
        now <- Sys.time()
        message(sprintf("\n#######################################################", appendLF=TRUE))
        message(sprintf("'xPierGenes' is being called to prioritise target genes (%s):", as.character(now)), appendLF=TRUE)
        message(sprintf("#######################################################", appendLF=TRUE))
    }
    
	pNode <- suppressMessages(xPierGenes(data=seeds.genes, network=network, weighted=weighted, network.customised=network.customised, normalise=normalise, restart=restart, normalise.affinity.matrix=normalise.affinity.matrix, parallel=parallel, multicores=multicores, verbose=verbose, RData.location=RData.location))
	
	if(verbose){
        now <- Sys.time()
        message(sprintf("#######################################################", appendLF=TRUE))
        message(sprintf("'xPierGenes' has been finished (%s)!", as.character(now)), appendLF=TRUE)
        message(sprintf("#######################################################\n", appendLF=TRUE))
    }
    
    pNode[['SNP']] <- df_SNP
    Gene2SNP <- xSM2DF(data=G2S_score, verbose=verbose)
    colnames(Gene2SNP) <- c('Gene','SNP','Score')
    pNode[['Gene2SNP']] <- Gene2SNP

	
    ####################################################################################
    endT <- Sys.time()
    if(verbose){
        message(paste(c("\nFinish at ",as.character(endT)), collapse=""), appendLF=TRUE)
    }
    
    runTime <- as.numeric(difftime(strptime(endT, "%Y-%m-%d %H:%M:%S"), strptime(startT, "%Y-%m-%d %H:%M:%S"), units="secs"))
    message(paste(c("Runtime in total is: ",runTime," secs\n"), collapse=""), appendLF=TRUE)
    
    invisible(pNode)
}
