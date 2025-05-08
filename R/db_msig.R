#' Get msigdbr package version number
#'
#' @return Version number
#'
#' @examples
#' msigdb_version()
#'
#' @export
msigdb_version <- function() {
    paste("v", as.character(packageVersion("msigdbr")), sep="")
}

#' Get msigdbr available species
#'
#' @return A character vector of species
#'
#' @examples
#' msigdb_species()
#'
#' @importFrom msigdbr msigdbr_species
#' @export
msigdb_species <- function() {
    msigdbr::msigdbr_species()$species_name
}

#' Check if species is available
#'
#' @param species A species
#' @return NULL
#'
#' @examples
#' \dontrun{
#' msigdb_check_species("Homo sapiens")
#' }
#' 
#' @keywords internal
msigdb_check_species <- function(species="") {
    if (!species %in% msigdb_species()) {
        stop("Species must be one of the following: \n", paste(msigdb_species(), "\n"))
    }
}

#' Get msigdbr available genesets
#'
#' @param species A species to determine gene symbols (refer to ?msigdbr::msigdbr for avilable species)
#' @return A dataframe of available genesets
#'
#' @examples
#' msigdb_available("Homo sapiens")
#'
#' @importFrom magrittr %>% set_colnames
#' @importFrom dplyr select
#' @importFrom msigdbr msigdbr
#' 
#' @export
msigdb_available <- function(species="Homo sapiens") {
    
    # Check species
    msigdb_check_species(species)
    
    # Gene set categories
    msigdbr(species=species) %>%
    dplyr::select(gs_cat, gs_subcat) %>%
    unique() %>%
    dplyr::arrange(gs_cat, gs_subcat) %>%
    magrittr::set_colnames(c("Category", "Subcategory"))
}

#' Print msigdb gsets information
#'
#' @return NULL
#'
#' @examples
#' msigdb_info()
#'
#' @export
msigdb_info <- function() {
    cat("|------------------------------------------------------------------|\n")
    cat("| Via: R package msigdbr                                    ")
    cat(msigdb_version())
    cat(" |\n")
    cat("|------------------------------------------------------------------|\n")
    cat("| Available Species                                                |\n")
    cat("|------------------------------------------------------------------|\n")
    cat("| Homo sapiens                                                     |\n")
    cat("| Mus musculus                                                     |\n")
    cat("| Drosophila melanogaster                                          |\n")
    cat("| Gallus gallus                                                    |\n")
    cat("| Saccharomyces cerevisiae                                         |\n")
    cat("| Bos taurus                                                       |\n")
    cat("| Caenorhabditis elegans                                           |\n")
    cat("| Canis lupus familiaris                                           |\n")
    cat("| Danio rerio                                                      |\n")
    cat("| Rattus norvegicus                                                |\n")
    cat("| Sus scrofa                                                       |\n")
    cat("|------------------------------------------------------------------|\n")
    cat("| Available Genesets                                               |\n")
    cat("|------------------------------------------------------------------|\n")
    cat("| Category Subcategory | Description                               |\n")    
    cat("|------------------------------------------------------------------|\n")
    cat("| C1                   | Positional                                |\n")
    cat("| C2 CGP               | Chemical and Genetic Perturbations        |\n")
    cat("| C2 CP                | Canonical Pathways                        |\n")
    cat("| C2 CP:BIOCARTA       | Canonical BIOCARTA                        |\n")
    cat("| C2 CP:KEGG           | Canonical KEGG                            |\n")
    cat("| C2 CP:PID            | Canonical PID                             |\n")
    cat("| C2 CP:REACTOME       | Canonical REACTOME                        |\n")
    cat("| C3 MIR               | Motif miRNA Targets                       |\n")
    cat("| C3 TFT               | Motif Transcription Factor Targets        |\n")
    cat("| C4 CGN               | Cancer Gene Neighborhoods                 |\n")
    cat("| C4 CM                | Cancer Modules                            |\n")
    cat("| C5 BP                | GO Biological Process                     |\n")
    cat("| C5 CC                | GO Cellular Component                     |\n")
    cat("| C5 MF                | GO Molecular Function                     |\n")
    cat("| C6                   | Oncogenic Signatures                      |\n")
    cat("| C7                   | Immunologic Signatures                    |\n")
    cat("| H                    | Hallmark                                  |\n")
    cat("|------------------------------------------------------------------|\n")
    cat("| Source: http://software.broadinstitute.org/gsea/msigdb           |\n")
    cat("|------------------------------------------------------------------|\n")
}

#' Download data from msigdb in the form of a named list
#'
#' @param species A species to determine gene symbols (refer to ?msigdbr::msigdbr for avilable species)
#' @param category Geneset category (refer to ?msigdbr::msigdbr for avilable categories)
#' @param subcategory Geneset subcategory (refer to ?msigdbr::msigdbr for avilable subcategories)
#' @return A list of genesets
#'
#' @examples
#' HALLMARK <- msigdb_download("Homo sapiens", "H", "")
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom msigdbr msigdbr
#' @export
msigdb_download <- function(species, category, subcategory="") {
    
    # Check species
    msigdb_check_species(species)
    
    response <- msigdbr(species, category, subcategory)
    if (nrow(response) == 0) {
        stop("No data found: Please review available species and genesets\n", msigdb_info())
    }
    
    # Download genesets
    mdf <- msigdbr(species, category, subcategory) %>%
        dplyr::select(gs_name, gene_symbol) %>%
        dplyr::distinct()
    
    # Convert to list
    gsets <- split(mdf$gene_symbol, mdf$gs_name)
    return(gsets)
}

#' Download data from msigdb in the form of a gsets object
#'
#' @param species A species to determine gene symbols (refer to ?msigdbr::msigdbr for avilable species)
#' @param category Geneset category (refer to ?msigdbr::msigdbr for avilable categories)
#' @param subcategory Geneset subcategory (refer to ?msigdbr::msigdbr for avilable subcategories)
#' @param clean Use true to clean labels of genesets
#' @return A gsets object
#'
#' @examples
#' HALLMARK <- msigdb_gsets("Homo sapiens", "H", "")
#'
#' @export
msigdb_gsets <- function(species, category, subcategory="", clean=FALSE) {
    genesets <- msigdb_download(species, category, subcategory)
    name <- ifelse(subcategory == "", category, paste(category, subcategory, sep="."))
    version <- msigdb_version()
    gsets$new(genesets, name=name, version=version, clean=clean)
}
