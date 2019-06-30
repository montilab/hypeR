# Expression set
library(Biobase)

edata <- read.table("inst/data-raw/edata.txt", sep = "\t", header=TRUE, stringsAsFactors=FALSE)
fdata <- read.table("inst/data-raw/fdata.txt", sep = "\t", header=TRUE, stringsAsFactors=FALSE)
pdata <- read.table("inst/data-raw/pdata.txt", sep = "\t", header=TRUE, stringsAsFactors=TRUE)

eset <- ExpressionSet(data.matrix(edata), AnnotatedDataFrame(pdata), AnnotatedDataFrame(fdata))

# Differential expression
require(limma)

design <- model.matrix(~ 0 + eset$class)
colnames(design) <- c("CARC", "NOCARC")
contrast.matrix <- makeContrasts(CARC-NOCARC, levels=design)

fit <- lmFit(eset, design)
fit <- contrasts.fit(fit, contrast.matrix)
fit <- eBayes(fit)

limma <- topTable(fit, adjust.method="BH", n=Inf, sort.by="P")
colnames(limma) <- c("symbol", "logFC", "AveExpr", "t", "pval", "fdr", "B")
limma <- limma[order(-limma$t),]

# Weighted Gene Co-Expression Analysis
library(WGCNA)

run_wgcna <- function(eset, class) {
    # Input
    counts <- t(exprs(eset[,eset$class == class]))
    
    # Choose a threshold which leads to best fit for scale-free topology
    threshold <- pickSoftThreshold(counts, powerVector=1:10)
    threshold <- threshold$fitIndices[threshold$fitIndices$SFT.R.sq == max(threshold$fitIndices$SFT.R.sq),'Power']    
    
    # Create adjacency matrix based on gene co-expression 
    adjacencyMatrix <- adjacency(counts, power=threshold) %>% TOMsimilarity()
    
    # Identify modules using dynamic tree cut
    modules <- hclust(as.dist(1-adjacencyMatrix), method="complete") %>%
               cutreeDynamic(distM=1-adjacencyMatrix, deepSplit=2, pamRespectsDendro=FALSE, minClusterSize=30)
    
    # Calculate eigengenes
    eigengenes <- moduleEigengenes(expr=counts, colors=modules)$eigengenes

    # Merge modules with similar eigengenes
    modules <- mergeCloseModules(exprData=counts, colors=modules, MEs=eigengenes, cutHeight=0.25)
    
    # Create a list of modules    
    modulesList <- vector("list", length=length(unique(modules$colors)))
    uniqueModules <- sort(unique(modules$colors))
    for(i in 1:length(unique(modules$colors))){
      pindex <- which(modules$colors==uniqueModules[i])
      modulesList[[i]] <- rownames(eset)[pindex]
    }
    names(modulesList) <- paste0('M',uniqueModules)
    
    return(modulesList)
}

wgcna <- list("NON-CARC" = run_wgcna(eset, class="NON-CARC"),
              "CARCINOGEN" = run_wgcna(eset, class="CARCINOGEN"))

hypdat <- list("limma"=limma, "wgcna"=wgcna)

saveRDS(hypdat, file.path(system.file("extdata", package="hypeR"), "hypdat.rds"))
