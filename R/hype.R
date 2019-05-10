#' Perform hyper enrichment
#' 
#' @param signature A vector of symbols
#' @param gsets A list of genesets or a relational genesets object
#' @param test Choose an enrichment type e.g. c("hypergeometric", "kstest")
#' @param bg Size or character vector of background population genes
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param weights_pwr Exponent for weights (Subramanian et al.)
#' @param absolute Takes max-min score rather than the max deviation from null
#' @param do_plots Use true to generate plots
#' @param verbose Use false to suppress logs
#' @return A hyp object
#'
#' @examples
#' gsets <- readRDS(file.path(system.file("extdata", package="hypeR"), "hypdat.rds"))$gsets
#'
#' signature <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'                "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'                "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' # Perform hyper enrichment
#' hyp_obj <- hypeR(signature, gsets, bg=2522, fdr_cutoff=0.05)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter arrange
#' @export
hypeR <- function(signature,
                  gsets,
                  test=c("hypergeometric", "kstest"),
                  bg=23467,
                  pval_cutoff=1,
                  fdr_cutoff=1,
                  weights_pwr=1,
                  absolute=FALSE,
                  do_plots=FALSE,
                  verbose=FALSE) {

    # Default arguments
    test <- match.arg(test)

    # Save original arguments
    args <- as.list(environment())
    
    # Check if gsets are relational
    if (is(gsets, "rgsets")) {
        gsets <- gsets$gsets
    }

    # Handling of multiple signatures
    if (is(signature, "list")) {
        lhyp <- lapply(signature, hypeR, args$gsets,
                                         args$test,
                                         args$bg,
                                         args$pval_cutoff,
                                         args$fdr_cutoff, 
                                         args$weights_pwr,
                                         args$absolute,
                                         args$do_plots,
                                         args$verbose)

        # Wrap list of hyp objects in multihyp object
        multihyp.obj <- multihyp$new(data=lhyp)
        return(multihyp.obj)
    }
    
    # Handling a background population
    if (is(bg, "character") & is(bg, "vector")) {
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

    # In case results are empty
    data <- data.frame(matrix(ncol=6, nrow=0))
    plots <- ggempty()

    # Overrepresentation test
    if (test == "hypergeometric") {
        colnames(data) <- c("label", "pval", "fdr", "gset.size", "genes.overlap", "hits")
        results <- .hyper_enrichment(signature, gsets, bg, do_plots)
    }
    
    # Enrichment test
    if (test == "kstest") {
        if (is(signature, "numeric")) {
            if (is.null(names(signature))) stop("Weighted signatures must be named")
            weights <- signature
            signature <- names(weights)
        } else {
          weights <- NULL
        }
        colnames(data) <- c("label", "pval", "fdr", "gset.size", "genes.found", "score")
        results <- .ks_enrichment(signature, gsets, weights, weights_pwr,absolute, do_plots)
    }

    # If hits are found format dataframe
    if (!is.null(results$data)) {
        
        data <- results$data %>%
                dplyr::filter(pval <= pval_cutoff) %>%
                dplyr::filter(fdr <= fdr_cutoff) %>%
                dplyr::arrange(pval)

        plots <- results$plots[data$label]
    }
    
    # Wrap dataframe in hyp object
    hyp_obj <- hyp$new(data=data, plots=plots, args=args)
    return(hyp_obj)
}
