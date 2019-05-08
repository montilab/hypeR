#' Perform hyper enrichment
#' 
#' @param signature A vector of symbols
#' @param gsets A list of genesets
#' @param test Choose an enrichment type e.g. c("hypergeometric", "kstest")
#' @param bg Size or character vector of background population genes
#' @param is_rgsets Use true to inform gsets are relational
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param weights Weights for weighted score (Subramanian et al.)
#' @param weights_pwr Exponent for weights (Subramanian et al.)
#' @param absolute Takes max-min score rather than the max deviation from null
#' @param do_plots Use true to generate plots
#' @param verbose Use false to suppress logs
#' @return A hyp object
#'
#' @examples
#' # Grab a list of curated gene sets
#' gsets <- readRDS(system.file("extdata/gsets.rds", package="hypeR"))
#' REACTOME <- gsets$REACTOME
#'
#' # Genes involed in tricarboxylic acid cycle
#' signature <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'                "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'                "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' # Perform hyper enrichment
#' hyp_obj <- hypeR(signature, REACTOME, bg=2522, fdr_cutoff=0.05)
#'
#' @importFrom magrittr %>%
#' @export
hypeR <- function(signature,
                  gsets,
                  test=c("hypergeometric", "kstest"),
                  bg=23467,
                  is_rgsets=FALSE,
                  pval_cutoff=1,
                  fdr_cutoff=1,
                  weights=NULL,
                  weights_pwr=1,
                  absolute=FALSE,
                  do_plots=FALSE,
                  verbose=FALSE) {

    # Default arguments
    test <- match.arg(test)

    # Save original arguments
    args <- as.list(environment())
    
    # Check if gsets are relational
    if (is_rgsets) {
        stopifnot(is(gsets, "rgsets"))
        gsets <- gsets$gsets
    }

    # Handling of multiple signatures
    if (is(signature, "list")) {
        if (is.null(weights)) {
            # Without weights run normally
            lhyp <- lapply(signature, hypeR, args$gsets,
                                             args$test,
                                             args$bg,
                                             args$is_rgsets, 
                                             args$pval_cutoff,
                                             args$fdr_cutoff, 
                                             args$weights,
                                             args$weights_pwr,
                                             args$absolute,
                                             args$do_plots,
                                             args$verbose)
        } else {
            # With weights they must be mapped to signatures
            stopifnot(length(signature) == length(weights))
            lhyp <- mapply(function(signature, weights) {
                        hypeR(signature,
                              gsets,
                              test,
                              bg,
                              is_rgsets, 
                              pval_cutoff,
                              fdr_cutoff, 
                              weights,
                              weights_pwr,
                              absolute,
                              do_plots,
                              verbose)
                    }, signature, weights)
        }
        # Wrap list of hyp objects in multihyp object
        multihyp.obj <- multihyp$new(data=lhyp)
        return(multihyp.obj)
    }
    
    # Handling a background population
    if ("character" %in% is(bg) & "vector" %in% is(bg)) {
        gsets <- lapply(gsets, function(x) intersect(x, bg))
        bg <- length(bg)
    }
    
    if (verbose) {
        cat("Signature size: ", length(signature), "\n")
        cat("Number of genesets: ", length(gsets), "\n")
        cat("Background population size: ", bg, "\n")
        cat("P-Value cutoff: ", pval_cutoff, "\n")
        cat("FDR cutoff:  ", fdr_cutoff, "\n")
    }

    if (test == "hypergeometric") {
        data <- data.frame(matrix(ncol=6, nrow=0))
        colnames(data) <- c("label", "pval", "fdr", "gset.size", "genes.overlap", "hits")
        plots <- ggempty()
        results <- .hyper_enrichment(signature, 
                                     gsets, 
                                     bg,
                                     do_plots)
    }
    
    if (test == "kstest") {
        data <- data.frame(matrix(ncol=6, nrow=0))
        colnames(data) <- c("label", "pval", "fdr", "gset.size", "genes.found", "score")
        plots <- ggempty()
        results <- .ks_enrichment(signature, 
                                  gsets, 
                                  weights,
                                  weights_pwr,
                                  absolute,
                                  do_plots)
    }

    # If hits are found format dataframe
    if (!is.null(results$data)) {
        data <- results$data %>%
                .[.$pval <= pval_cutoff,,drop=FALSE] %>%
                .[.$fdr <= fdr_cutoff,,drop=FALSE] %>%
                .[order(.$pval),,drop=FALSE]
            
        plots <- results$plots[data$label]
    }
    
    # Wrap dataframe in hyp object
    hyp_obj <- hyp$new(data=data, plots=plots, args=args)
    return(hyp_obj)
}
