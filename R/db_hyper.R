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
              .send_headers = c("Accept" = "application/octet-stream"),
              httr::write_disk(temp, overwrite=TRUE))    
    return(readRDS(temp))
}
