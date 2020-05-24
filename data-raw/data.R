library(Biobase)
library(limma)
library(usethis)
library(magrittr)
library(dplyr)
library(shine)

# Very basic differential expression example
eset <- readRDS("data-raw/raw/CARCINOGENICITY-RNA-Seq-Expression-Set.rds")
design <- model.matrix(~ 0 + eset$class)
colnames(design) <- c("CARC", "NOCARC")
contrast.matrix <- makeContrasts(CARC-NOCARC, levels=design)

fit <- lmFit(eset, design)
fit <- contrasts.fit(fit, contrast.matrix)
fit <- eBayes(fit)

limma <- topTable(fit, adjust.method="BH", n=Inf, sort.by="P")
limma <- limma %>%
         dplyr::select("symbol", "logFC", "t", "P.Value", "adj.P.Val") %>%
         magrittr::set_colnames(c("symbol", "lfc", "t", "pval", "fdr")) %>%
         dplyr::mutate_if(function(x) {is.numeric(x) && x >= 1}, round, digits=2) %>%
         dplyr::mutate_if(function(x) {is.numeric(x) && x < 1}, signif, digits=3)
    
usethis::use_data(limma)

# Very basic co-expression modules example
eset <- readRDS("data-raw/raw/TCGA-BRCA-RNA-Seq-Expression-Set.rds")
wgcna.wrapper <- function(eset, subtype) {
    genes.var <- shine::keep.var(eset, column="subtype", subtypes=subtype, fn=mad)
    genes.mad <- shine::rank.var(eset, column="subtype", subtypes=subtype, limit=10000, genes=genes.var, fn=mad)    
    eset.subtype <- eset[genes.mad, eset$subtype == subtype]
    results <- shine::mods.detect(eset.subtype, min.size=30, cores=3, cor.fn="bicor", merging=TRUE)
    mods <- results$mods
    mods$grey <- NULL
    return(mods)
}

wgcna <- list("LA" = wgcna.wrapper(eset, subtype="LA"),
              "LB" = wgcna.wrapper(eset, subtype="LB"),
              "H2" = wgcna.wrapper(eset, subtype="H2"),
              "BL" = wgcna.wrapper(eset, subtype="BL"))

usethis::use_data(wgcna)
