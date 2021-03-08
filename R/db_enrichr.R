#' Get url base for species-specific enrichr libraries
#'
#' @param db A species
#' @return A url
#' 
#' @keywords internal
enrichr_urls <- function(db=c("Enrichr", "YeastEnrichr", "FlyEnrichr", "WormEnrichr", "FishEnrichr")) {
    switch(match.arg(db),
        "Enrichr"      = "http://maayanlab.cloud/Enrichr/{1}",
        "YeastEnrichr" = "http://maayanlab.cloud/YeastEnrichr/{1}",
        "FlyEnrichr"   = "http://maayanlab.cloud/FlyEnrichr/{1}",
        "WormEnrichr"  = "http://maayanlab.cloud/WormEnrichr/{1}",
        "FishEnrichr"  = "http://maayanlab.cloud/FishEnrichr/{1}"
    )
}

#' Connect to the enrichr web application
#'
#' @param endpoint The url endpoint to connect to
#' @param db A species
#' @return A web response
#'
#' @importFrom httr GET http_status
#' 
#' @keywords internal
enrichr_connect <- function(endpoint, db=c("Enrichr", "YeastEnrichr", "FlyEnrichr", "WormEnrichr", "FishEnrichr")) {
    url <- enrichr_urls(db)
    response <- httr::GET(.format_str(url, endpoint))   
    if (!http_status(response)$category == "Success") {
        stop(http_status(response)$message)
    }
    return(response)
}

#' Get enrichr available genesets
#'
#' @param db A species
#' @return A dataframe of available genesets
#'
#' @examples
#' enrichr_available()
#' 
#' @importFrom httr content
#' @importFrom magrittr %>% set_colnames
#' @importFrom dplyr select
#' 
#' @export
enrichr_available <- function(db=c("Enrichr", "YeastEnrichr", "FlyEnrichr", "WormEnrichr", "FishEnrichr")) {
    response <- enrichr_connect("datasetStatistics", db)
    data <- content(response, "parsed")  
    data$statistics %>%
    lapply(function(x) do.call(cbind.data.frame, x)) %>%
    do.call(rbind.data.frame, .) %>%
    dplyr::select("libraryName", "numTerms", "geneCoverage", "genesPerTerm", "link") %>%
    magrittr::set_colnames(c("Geneset", "Set Number", "Coverage", "Genes Per Set", "Source"))
}

#' Download data from enrichr in the form of a named list
#'
#' @param genesets A name corresponding to available genesets
#' @param db A species
#' @return A list of genesets
#'
#' @examples
#' ATLAS <- enrichr_download("Human_Gene_Atlas")
#' 
#' @importFrom httr content
#' 
#' @export
enrichr_download <- function(genesets, db=c("Enrichr", "YeastEnrichr", "FlyEnrichr", "WormEnrichr", "FishEnrichr")) {
    response <- enrichr_connect(.format_str("geneSetLibrary?mode=text&libraryName={1}", genesets), db)
    data <- content(response, "text")
    split <- strsplit(data, split="\n")[[1]]
    genesets <- sapply(split, function(x) strsplit(x, "\t")[[1]])
    names(genesets) <- unlist(lapply(genesets, function(x) x[1]))
    lapply(genesets, function(x) {
        genes <- x[3:length(x)]
        genes <- genes[genes != ""]
        unique(genes)
    })
}

#' Download data from enrichr in the form of a gsets object
#'
#' @param genesets A name corresponding to available genesets
#' @param db A species
#' @param clean Use true to clean labels of genesets
#' @return A gsets object
#'
#' @examples
#' ATLAS <- enrichr_gsets("Human_Gene_Atlas")
#' 
#' @importFrom httr content
#' 
#' @export
enrichr_gsets <- function(genesets, db=c("Enrichr", "YeastEnrichr", "FlyEnrichr", "WormEnrichr", "FishEnrichr"), clean=FALSE) {
    name <- .format_str("{1} ({2})", genesets, match.arg(db))
    genesets <- enrichr_download(genesets, db)
    version <- .format_str("Downloaded: {1}", Sys.Date())
    gsets$new(genesets, name=name, version=version, clean=clean)
}
