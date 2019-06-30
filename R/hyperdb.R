#' Print hypeR-db gsets
#'
#' @param type Select the type of geneset desired e.g. c("gsets", "rgsets")
#' @param quiet Use true to suppress printing of available genesets
#' @return A character vector of available genesets
#'
#' @examples
#' available <- hyperdb_info(type="gsets", quiet=TRUE)
#' available <- hyperdb_info(type="rgsets", quiet=TRUE)
#'
#' @importFrom gh gh
#' @export
hyperdb_info <- function(type=c("gsets", "rgsets"), quiet=FALSE) {
    type <- match.arg(type)
    cat("Getting available", type, "...", "\n")
    response <- gh("/repos/:owner/:repo/contents/:path", 
                   owner="montilab", 
                   repo="hyperdb", 
                   path=file.path("data", type))
    files <- vapply(response, "[[", "", "name")
    gsets <- gsub(".rds", "", files)
    if (!quiet) print(gsets) 
    return(gsets)
}

#' Fetch gsets from hypeR-db
#'
#' @param type Select the type of geneset desired e.g. c("gsets", "rgsets")
#' @param gset A name corresponding to an available geneset
#' @return A list of gene sets
#'
#' @examples
#' gsets <- hyperdb_fetch(type="gsets", "KEGG_2019_Human")
#' rgsets <- hyperdb_fetch(type="rgsets", "KEGG")
#'
#' @export
hyperdb_fetch <- function(type=c("gsets", "rgsets"), gset) {
    type <- match.arg(type)
    path <- file.path("https://github.com/montilab/hypeR-db/raw/master/data", type)
    url <- file.path(path, "{0}.rds")
    temp <- tempfile(fileext=".rds")
    httr::GET(gsub("\\{0}", gset, url), 
              .send_headers = c("Accept" = "application/octet-stream"),
              httr::write_disk(temp, overwrite=TRUE))    
    return(readRDS(temp))
}
