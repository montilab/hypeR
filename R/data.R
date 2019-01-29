#' Fetch a gene sets from msigdb
#'
#' @param species A species to determine gene symbols 
#' @param category Gene set category
#' @param subcategory Gene set subcategory
#' @return A list of gene sets
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom msigdbr msigdbr
download.gsets <- function(species, category, subcategory) {

    # Download genesets
    mdf <- msigdbr(species, category, subcategory) %>%
           dplyr::select(gs_name, gene_symbol) %>%
           as.data.frame() %>%
           stats::aggregate(gene_symbol ~ gs_name, data=., c)

    # Convert to list
    gsets <- as.list(mdf$gene_symbol)
    names(gsets) <- mdf$gs_name
    return(gsets)
}

#' Update available gene sets from msigdb
#'
#' @param species A species to determine gene symbols 
#' @return None
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select arrange
#' @importFrom msigdbr msigdbr
update.gsets <- function(species="Homo sapiens") {

    cat("Downloading genesets...\n")

    # Gene set categories
    res <- msigdbr(species=species) %>%
           dplyr::select(gs_cat, gs_subcat) %>%
           unique() %>%
           dplyr::arrange(gs_cat, gs_subcat)

    # Version of data
    version <- packageVersion("msigdbr")

    for(i in 1:nrow(res)) {
        category <- as.character(res[i,1])
        subcategory <- as.character(res[i,2])

        # Download gsets
        gsets <- download.gsets(species, category, subcategory)

        # Filename formatting
        nm <- ifelse(subcategory == "", category, paste(category, subcategory, sep="."))
        nm <- gsub(":", ".", nm)
        vs <- paste("v", version, sep="")
        fn <- paste(nm, vs, 'rds', sep=".")

        # Logging
        cat(paste("-", nm, "->", length(gsets), "Gene Sets", "\n"))

        db <- file.path(system.file("extdata", package="hypeR"), fn)
        saveRDS(gsets, db)
    }
}

#' Print available gene sets
#'
#' @return None
#'
#' @examples
#' db.info()
#'
#' @export
db.info <- function() {
    cat("|------------------------------------------------------------|\n")
    cat("| Available Gene Sets                                 v6.2.1 |\n")
    cat("|------------------------------------------------------------|\n")
    cat("| C1             | Positional (326)                          |\n")
    cat("| C2.CGP         | Chemical and Genetic Perturbations (3433) |\n")
    cat("| C2.CP          | Canonical Pathways (252)                  |\n")
    cat("| C2.CP.BIOCARTA | Canonical BIOCARTA (217)                  |\n")
    cat("| C2.CP.KEGG     | Canonical KEGG (186)                      |\n")
    cat("| C2.CP.REACTOME | Canonical REACTOME (674)                  |\n")
    cat("| C3.MIR         | Motif miRNA Targets (221)                 |\n")
    cat("| C3.TFT         | Motif Transcription Factor Targets (615)  |\n")
    cat("| C4.CGN         | Cancer Gene Neighborhoods (427)           |\n")
    cat("| C4.CM          | Cancer Modules (431)                      |\n")
    cat("| C5.BP          | GO Biological Process (4436)              |\n")
    cat("| C5.CC          | GO Cellular Component (580)               |\n")
    cat("| C5.MF          | GO Molecular Function (901)               |\n")
    cat("| C6             | Oncogenic Signatures (189)                |\n")
    cat("| C7             | Immunologic Signatures (4872)             |\n")
    cat("| H              | Hallmark (50)                             |\n")
    cat("|------------------------------------------------------------|\n")
    cat("| Source: http://software.broadinstitute.org/gsea/msigdb     |\n")
    cat("|------------------------------------------------------------|\n")
}

#' Fetch gene sets
#'
#' @param symbol A symbol corresponding to a gene set
#' @return A list of gene sets
#'
#' @examples
#' C1 <- db.get("C1")
#'
#' @export
db.get <- function(symbol) {
    path <- paste(symbol, "v6.2.1.rds", sep=".")
    db <- system.file("extdata", path, package="hypeR")
    gs <- readRDS(db)
    return(gs)
}