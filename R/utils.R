#' Clean labels of genesets
#' 
#' @param x A vector of labels
#' 
#' @examples
#' HALLMARK <- msigdb_download("Homo sapiens", "H", "")
#' names(HALLMARK) <- clean_genesets(names(HALLMARK))
#' head(names(HALLMARK))
#' 
#' @importFrom stringr str_to_sentence
#' 
#' @export
clean_genesets <- function(x) {
    # Longest leading common substring
    x.split <- strsplit(x, '')
    x.split <- lapply(x.split, `length<-`, max(nchar(x)))
    x.mat <- do.call(rbind, x.split)
    csl <- which.max(apply(x.mat, 2, function(col) !length(unique(col)) == 1)) - 1
    lcls <- substr(x[1], 1, csl)
    
    # Clean
    x <- gsub(lcls, "", x)
    x <- gsub("_", " ", x)
    x <- stringr::str_to_title(x)
    return(x)
}

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
        warning(.format_str("Only {1}% of signature was found across genesets", overlap*100))
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

#' Adjust alpha of a hex string
#'
#' @param hex A 6-character hex string (e.g. #000000)
#' @param percent Alpha level from 0-1
#' @return A hex string
#' 
#' @keywords internal
.hexa <- function(hex, percent=1) {
    if (percent < 0) percent <- 0
    if (percent > 1) percent <- 1
    percent <- toupper(as.hexmode(floor(percent * 255)))
    percent <- sprintf("%02s", percent)
    hex <- paste0(hex, percent)
    return(hex)
}