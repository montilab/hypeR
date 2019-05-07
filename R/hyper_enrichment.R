#' Overrepresentation test via hyper-geometric distribution
#'
#' @param signature A vector of symbols
#' @param gsets A list of genesets
#' @param bg Size of background population genes
#' @param do.plots Use true to generate plots
#' @return A list of data and plots
#'
#' @importFrom stats phyper p.adjust
#' @keywords internal
.hyper_enrichment <- function(signature,
                              gsets,
                              bg=length(unique(unlist(gsets))),
                              do.plots=TRUE) {

    if (!is(signature, "vector")) stop("Error: Expected signature to be a vector of symbols\n")
    if (!is(gsets, "list")) stop("Error: Expected gsets to be a list of gene sets\n")
    
    signature.found <- signature[signature %in% unique(unlist(gsets))]

    n.hits <- sapply(gsets, function(x, y) length(intersect(x, y)), signature.found)
    n.drawn <- length(signature)
    n.gsets <- sapply(gsets, length)
    n.left <- bg-n.gsets
    
    pvals <- suppressWarnings(stats::phyper(q=n.hits-1,
                                            m=n.gsets,
                                            n=n.left,
                                            k=n.drawn,
                                            lower.tail=FALSE))
    
    data <- data.frame(pval=signif(pvals, 2),
                       fdr=signif(stats::p.adjust(pvals, method="fdr"), 2),
                       gset.size=n.gsets,
                       genes.overlap=n.hits,
                       hits=sapply(gsets, function(x, y) paste(intersect(x, y), collapse=','), signature.found))
    
    if (do.plots) {
        plots <- lapply(seq_len(length(gsets)), function(i) {
                     a <- signature
                     b <- gsets[[i]]
                     ga <- "Signature"
                     gb <- "Geneset"
                     title <- names(gsets)[[i]]
                     ggvenn(a, b, ga, gb, title)
                 })
        
        names(plots) <- names(gsets)
    } else {
        plots <- lapply(gsets, function(x) {ggempty()})
    }
    
    return(list(data=data, plots=plots))
}
