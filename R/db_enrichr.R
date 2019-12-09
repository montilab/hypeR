#' Connect to the enrichr web application
#'
#' @param endpoint The url endpoint to connect to
#' @return A web response
#'
#' @importFrom httr GET http_status
#' 
#' @keywords internal
enrichr_connect <- function(endpoint) {
    url <- "http://amp.pharm.mssm.edu/Enrichr/{1}"
    response <- httr::GET(.format_str(url, endpoint))   
    if (!http_status(response)$category == "Success") {
        stop(http_status(response)$message)
    }
    return(response)
}

#' Get enrichr available genesets
#'
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
enrichr_available <- function() {
    response <- enrichr_connect("datasetStatistics")
    data <- content(response, "parsed")  
    data$statistics %>%
    lapply(function(x) do.call(cbind.data.frame, x)) %>%
    do.call(rbind.data.frame, .) %>%
    magrittr::set_colnames(c("Coverage", "Genes Per Set", "Geneset", "Source", "Set Number")) %>%
    dplyr::select(c("Geneset", "Set Number", "Coverage", "Genes Per Set", "Source"))
}

#' Download data from enrichr in the form of a named list
#'
#' @param genesets A name corresponding to available genesets
#' @return A list of genesets
#'
#' @examples
#' ATLAS <- enrichr_download("Human_Gene_Atlas")
#' 
#' @importFrom httr content
#' 
#' @export
enrichr_download <- function(genesets) {
    response <- enrichr_connect(.format_str("geneSetLibrary?mode=text&libraryName={1}", genesets))
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
#' @return A gsets object
#'
#' @examples
#' ATLAS <- enrichr_gsets("Human_Gene_Atlas")
#' 
#' @importFrom httr content
#' 
#' @export
enrichr_gsets <- function(genesets) {
    name <- genesets
    genesets <- enrichr_download(genesets)
    version <- .format_str("Downloaded: {1}", Sys.Date())
    gsets$new(genesets, name=name, version=version)
}
