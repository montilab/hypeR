#' Get base url for hyperdb
#' 
#' @return A base url
#'
#' @keywords internal
.hyperdb_url <- function(api=FALSE) {
    if (api) {
        return("https://api.github.com/repos/montilab/hyperdb")
    } else {
        return("https://github.com/montilab/hyperdb")   
    }
}

#' Load an rds file directly from github
#' 
#' @param url A url
#' @return A list
#'
#' @importFrom httr GET
#'
#' @keywords internal
.github_rds <- function(url) {
    temp <- tempfile(fileext=".rds")
    httr::GET(url, .send_headers = c("Accept"="application/octet-stream"), httr::write_disk(temp, overwrite=TRUE))    
    return(readRDS(temp))
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
    base <- .hyperdb_url(api=TRUE)
    url <- .format_str("{1}/git/trees/master?recursive=1", base)
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
    base <- .hyperdb_url()
    url <- .format_str("{1}/raw/master/data/{2}/{3}", base, source, gsets)
    .github_rds(url)
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
    base <- .hyperdb_url()
    url <- .format_str("{1}/raw/master/data/{2}/{2}_v{3}.rds", base, rgsets, version)
    .github_rds(url)
}
