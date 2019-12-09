#' Overrepresentation test via hyper-geometric distribution
#'
#' @param signature A vector of symbols
#' @param genesets A list of genesets
#' @param background Size of background population genes
#' @param plotting Use true to generate plots
#' @return A list of data and plots
#'
#' @importFrom stats phyper p.adjust
#' @keywords internal
.hyper_enrichment <- function(signature,
                              genesets,
                              background=length(unique(unlist(genesets))),
                              plotting=TRUE) {

    if (!is(signature, "vector")) stop("Expected signature to be a vector of symbols\n")
    if (!is(genesets, "list")) stop("Expected genesets to be a list of genesets\n")
    
    signature <- unique(signature)
    genesets <- lapply(genesets, unique)
    
    # Construct table
    signature.found <- signature[signature %in% unique(unlist(genesets))]
    n.hits <- sapply(genesets, function(x, y) length(intersect(x, y)), signature.found)
    n.drawn <- length(signature)
    n.genesets <- sapply(genesets, length)
    n.left <- background-n.genesets
    
    # Hypergeometric test
    pvals <- suppressWarnings(stats::phyper(q=n.hits-1,
                                            m=n.genesets,
                                            n=n.left,
                                            k=n.drawn,
                                            lower.tail=FALSE))
    
    # Format data
    data <- data.frame(label=names(genesets),
                       pval=signif(pvals, 2),
                       fdr=signif(stats::p.adjust(pvals, method="fdr"), 2),
                       signature=length(signature),
                       geneset=n.genesets,
                       overlap=n.hits,
                       background=background,
                       hits=sapply(genesets, function(x, y) paste(intersect(x, y), collapse=','), signature.found),
                       stringsAsFactors=FALSE)
    
    # Handle plots
    if (plotting) {
        plots <- mapply(function(geneset, title) {
            ggvenn(signature, geneset, "Signature", "Geneset", title)
        }, genesets, names(genesets), USE.NAMES=TRUE, SIMPLIFY=FALSE)
    } else {
        plots <- lapply(genesets, function(x) {ggempty()})
    }
    
    return(list(data=data, plots=plots))
}
