#' Check overlap of signature across genesets
#' 
#' @param signature A vector of symbols
#' @param genesets A list of genesets
#' @param threshold Minimum percent overlap
#' @return Percent overlap
#'
#' @keywords internal
.check_overlap <- function(signature, genesets, threshold=0.05) {
    overlap <- signif(length(intersect(signature, unique(unlist(genesets)))) / length(signature), 2)
    if (overlap < threshold) {
        warning(.format_str("Only {1}% of signature was found across genesets", overlap))
    }
    return(overlap)
}

#' Format a string using placeholders
#'
#' @param string A an unformatted string with placeholders
#' @param ... Variables to format placeholders with
#' @return A formatted string
#' 
#' @examples
#' \dontrun{
#' format_str("Format with {1} and {2}", "x", "y")
#' }
#'
#' @keywords internal
.format_str <- function(string, ...) {
    args <- list(...)
    for (i in 1:length(args)) {
        pattern <- paste("\\{", i, "}", sep="")
        replacement <- args[[i]]
        string <- gsub(pattern, replacement, string)
    }
    return(string)
}

#' Convert an arguments list to string format
#'
#' @param args A list of keyword arguments
#' @return A string of keyword arguments
#'
#' @examples
#' \dontrun{
#' string_args(list(x=15, y="fdr", z=TRUE))
#' }
#'
#' @keywords internal
.string_args <- function(args) {
    paste(paste(names(args), 
                sapply(unname(args), function(x) {
                    if (is.character(x)) {shQuote(x)}
                    else if (is.null(x)) {"NULL"}
                    else {x}
                }), 
                sep='='), 
                collapse=",")
}

#' Calculate jaccard similarity of two sets
#'
#' @param a A vector
#' @param b A vector
#' @return A numerical value
#'
#' @keywords internal
.jaccard_similarity <- function(a, b) {
    length( intersect(a, b) ) / length( union(a, b) )
}

#' Calculate overlap similarity of two sets
#'
#' @param a A vector
#' @param b A vector
#' @return A numerical value
#'
#' @keywords internal
.overlap_similarity <- function(a, b) {
    length( intersect(a, b) ) / min( length(a), length(b) )
}

#' Custom reverse log transformation of continous ggplot axes
#'
#' @param base Logarithm base
#' @importFrom scales trans_new log_breaks
#' 
#' @keywords internal
.reverselog_trans <- function(base=exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    scales::trans_new(paste0("reverselog-", format(base)), trans, inv, 
              scales::log_breaks(base=base), 
              domain=c(1e-100, Inf))
}

#' Find geneset members
#'
#' @param id A vector of ids
#' @param genesets A list of genesets (see \code{rgsets})
#' @param nodes A data frame of labeled nodes (see \code{rgsets})
#' @param edges A data frame of directed edges (see \code{rgsets})
#' @return A vector of ids
#'
#' @importFrom dplyr filter pull %>%
#' 
#' @keywords internal
.find_members <- function(id, genesets, nodes, edges) {
    label <- nodes[id, "label"]
    if (label %in% names(genesets)) {
        genesets[[label]]
    } else {
        edges %>%
        dplyr::filter(from == id) %>%
        dplyr::pull(to) %>%
        lapply(.find_members, genesets, nodes, edges) %>%
        unlist() %>%
        unique()
    }
}