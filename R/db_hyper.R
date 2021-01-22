#' Print hypeR-db rgsets information
#'
#' @return NULL
#'
#' @examples
#' hyperdb_info()
#'
#' @export
hyperdb_info <- function() {
    cat("REACTOME\n")
    cat("Versions: 70.0\n")
    cat("KEGG\n")
    cat("Versions: 92.0")
}

#' Check available data to download from hyperdb
#' 
#' @examples
#' hyperdb_available()
#'
#' @importFrom httr GET stop_for_status
#' @importFrom reshape2 melt
#' @importFrom magrittr set_colnames
#' @importFrom dplyr relocate
#'
#' @export
hyperdb_available <- function() {
    base <- "https://api.github.com/repos/montilab/hypeRdb/git/trees/master"
    url <- .format_str("{1}?recursive=1", base)
    response <- httr::GET(url)
    httr::stop_for_status(response)
    repo <- unlist(lapply(content(response)$tree, "[", "path"), use.names=F)
    fl <- repo[grepl("^data/.", repo)]
    ll <- list()
    for (f in fl) {
        name <- NULL
        split <- strsplit(f , "/")[[1]]
        if (length(split) > 2){
            name <- split[2]
            rds <- split[3]
            if (!name %in% names(ll)) {
                ll[[name]] <- c(rds)
            } else {
                ll[[name]] <- c(ll[[name]], rds)
            }
        }
    }
    ll %>%
    reshape2::melt() %>%
    magrittr::set_colnames(c("gsets", "source")) %>%
    dplyr::relocate(source, gsets)
}

#' Download data from hyperdb
#'
#' @param source A source identifier
#' @param gsets A genesets identifier
#' @return A list
#'
#' @examples
#' KEGG <- hyperdb_gsets("KEGG", "KEGG_v92.0.rds")
#'
#' @importFrom httr GET
#'
#' @export
hyperdb_gsets <- function(source, gsets) {
    base <- "https://github.com/montilab/hyperdb/raw/master/data"
    url <- .format_str("{1}/{2}/{3}", base, source, gsets)
    temp <- tempfile(fileext=".rds")
    httr::GET(gsub("\\{0}", gsets, url), 
              .send_headers = c("Accept"="application/octet-stream"),
              httr::write_disk(temp, overwrite=TRUE))    
    return(readRDS(temp))
}

#' Download data from hyperdb in the form of a rgsets object
#'
#' @param rgsets A name corresponding to an available relational genesets object
#' @param version A version number
#' @return An rgsets object
#'
#' @examples
#' REACTOME <- hyperdb_rgsets("REACTOME", "70.0")
#'
#' @importFrom httr GET
#'
#' @export
hyperdb_rgsets <- function(rgsets, version) {
    base <- "https://github.com/montilab/hyperdb/raw/master/data"
    file <- .format_str("{1}/{1}_v{2}.rds", rgsets, version)
    url <- .format_str("{1}/{2}", base, file)
    temp <- tempfile(fileext=".rds")
    httr::GET(gsub("\\{0}", rgsets, url), 
              .send_headers = c("Accept"="application/octet-stream"),
              httr::write_disk(temp, overwrite=TRUE))    
    return(readRDS(temp))
}
