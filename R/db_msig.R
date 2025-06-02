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
#' @param species A species to determine gene symbols (refer to ?msigdbr::msigdbr for available species)
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
    dplyr::select(gs_collection, gs_subcollection) %>%
    unique() %>%
    dplyr::arrange(gs_collection, gs_subcollection) %>%
    magrittr::set_colnames(c("Collection", "Subcollection"))
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
    cat("| Collection Subcollection | Description                               |\n")    
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
#' @param species A species to determine gene symbols (refer to ?msigdbr::msigdbr for available species)
#' @param collection Geneset collection (refer to ?msigdbr::msigdbr_collections for available categories)
#' @param subcollection Geneset subcollection (refer to ?msigdbr::msigdbr_collections for available subcategories)
#' @return A list of genesets
#'
#' @examples
#' HALLMARK_HUMAN <- msigdb_download("Homo sapiens", "H")
#' HALLMARK_MOUSE <- msigdb_download("Mus musculus", "MH")
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom msigdbr msigdbr
#' @export
msigdb_download <- function(species, collection, subcollection=NULL) {
    
    # Check species
    msigdb_check_species(species)
    
    if(species == "Homo sapiens") {
      db_species = "HS"
    } else if (species == "Mus musculus") {
      db_species = "MM"
    } else {
      # For non-human species msigdb will use gene orthologs from the human database.
      db_species == "HS"
    }
    
    response <- msigdbr(species = species, 
                        db_species = db_species, 
                        collection = collection, 
                        subcollection = subcollection)
    if (nrow(response) == 0) {
        stop("No data found: Please review available species and genesets\n", msigdb_info())
    }
    
    # Download genesets
    mdf <- msigdbr(species = species, 
                   collection = collection, 
                   db_species = db_species, 
                   subcollection = subcollection) %>%
        dplyr::select(gs_name, gene_symbol) %>%
        dplyr::distinct()
    
    # Convert to list
    gsets <- split(mdf$gene_symbol, mdf$gs_name)
    return(gsets)
}

#' Download data from msigdb in the form of a gsets object
#'
#' @param species A species to determine gene symbols (refer to ?msigdbr::msigdbr_species for available species)
#' @param collection Geneset collection (refer to ?msigdbr::msigdbr_collections for available categories)
#' @param subcollection Geneset subcollection (refer to ?msigdbr::msigdbr_collections for available subcategories)
#' @param clean Use true to clean labels of genesets
#' @return A gsets object
#'
#' @examples
#' HALLMARK_HUMAN <- msigdb_gsets("Homo sapiens", "H")
#' HALLMARK_MOUSE <- msigdb_gsets("Mus musculus", "MH")
#'
#' @export
msigdb_gsets <- function(species, collection, subcollection=NULL, clean=FALSE) {
    genesets <- msigdb_download(species, collection, subcollection)
    name <- ifelse(is.null(subcollection), collection, paste(collection, subcollection, sep="."))
    version <- msigdb_version()
    gsets$new(genesets, name=name, version=version, clean=clean)
}
