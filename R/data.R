#' Fetch a gene sets from msigdb
#'
#' @param species A species to determine gene symbols (refer to the msigdbr R package for available species)
#' @param category Gene set category (refer to the msigdbr R package for available categories)
#' @param subcategory Gene set subcategory (refer to the msigdbr R package for available subcategories)
#' @return A list of gene sets
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom msigdbr msigdbr
#'
#' @examples
#' HALLMARK <- download_gsets("Homo sapiens", "H", "")
#'
#' @export
download_gsets <- function(species="Homo sapiens", category, subcategory) {

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
#' @param species A species to determine gene symbols (refer to the msigdbr R package for available species)
#' @param output_dir A directory path where gene sets are downloaded instead of to the package location
#' @return None
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select arrange
#' @importFrom msigdbr msigdbr
#'
#' @export
download_msigdb <- function(species="Homo sapiens", output_dir=NULL) {

    if (is.null(output_dir)) {
        output_dir <- system.file("extdata", package="hypeR")
    } else {
        dir.create(output_dir, showWarnings=FALSE)
    }

    cat("Downloading genesets to...\n")
    cat(output_dir)
    cat("\n")

    # Gene set categories
    res <- msigdbr(species=species) %>%
           dplyr::select(gs_cat, gs_subcat) %>%
           unique() %>%
           dplyr::arrange(gs_cat, gs_subcat)

    # Version of data
    version <- packageVersion("msigdbr")

    for (i in seq_len(nrow(res))) {
        category <- as.character(res[i,1])
        subcategory <- as.character(res[i,2])

        # Download gsets
        gsets <- download_gsets(species, category, subcategory)

        # Filename formatting
        nm <- ifelse(subcategory == "", category, paste(category, subcategory, sep="."))
        nm <- gsub(":", ".", nm)
        vs <- paste("v", version, sep="")
        fn <- paste(nm, vs, 'rds', sep=".")

        # Logging
        cat(paste("-", nm, "->", length(gsets), "Gene Sets", "\n"))

        db <- file.path(output_dir, fn)
        saveRDS(gsets, db)
    }
}

#' Print available gene sets
#'
#' @return A table of values
#'
#' @examples
#' db_info()
#'
#' @export
db_info <- function() {
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
#' @param symbol A symbol corresponding to a gene set (hypeR comes with C2.CP.BIOCARTA, C2.CP.KEGG, and C2.CP.REACTOME, see vignette to download more)
#' @return A list of gene sets
#'
#' @examples
#' REACTOME <- db_get("C2.CP.REACTOME")
#'
#' @export
db_get <- function(symbol) {
    path <- paste(symbol, "v6.2.1.rds", sep=".")
    db <- system.file("extdata", path, package="hypeR")
    gs <- readRDS(db)
    return(gs)
}
