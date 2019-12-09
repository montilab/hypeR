#' Calculate enrichment of one or more signatures
#' 
#' @param signature A vector of symbols
#' @param genesets A gsets/rgsets object or a named list of genesets
#' @param test Choose an enrichment type e.g. c("hypergeometric", "kstest")
#' @param background Size or character vector of background population genes
#' @param power Exponent for weights (kstest only)
#' @param absolute Takes max-min score rather than the max deviation from null (kstest only)
#' @param pval Filter results to be less than pval cutoff
#' @param fdr Filter results to be less than fdr cutoff
#' @param plotting Use true to generate plots for each geneset test (may slow performance)
#' @param quiet Use true to suppress logs and warnings
#' @return A hyp object
#'
#' @examples
#' genesets <- msigdb_gsets("Homo sapiens", "C2", "CP:KEGG")
#'
#' signature <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'                "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'                "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' hyp_obj <- hypeR(signature, genesets, background=2522)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter arrange
#' 
#' @export
hypeR <- function(signature,
                  genesets,
                  test=c("hypergeometric", "kstest"),
                  background=23467,
                  power=1,
                  absolute=FALSE,
                  pval=1,
                  fdr=1,
                  plotting=FALSE,
                  quiet=TRUE) {

    # Handling of multiple signatures
    if (is(signature, "list")) {
        
        if (is.null(names(signature))) {
            stop("Lists of signatures must be named")
        }

        lhyp <- mapply(function(x, id) {
            cat(.format_str("{1}\n", id))

            hypeR(x,
                  genesets=genesets, 
                  test=test, 
                  background=background, 
                  power=power, 
                  absolute=absolute,
                  pval=pval,
                  fdr=fdr,
                  plotting=plotting, 
                  quiet=quiet)

        }, signature, names(signature), USE.NAMES=TRUE, SIMPLIFY=FALSE)

        # Wrap list of hyp objects in multihyp object
        return(multihyp$new(data=lhyp))
    }

    # Save original arguments
    args <- as.list(environment())

    # Default arguments
    test <- match.arg(test)
    args$test <- test

    # Handle gsets
    if (is(genesets, "list")) {
        gsets.obj <- gsets$new(genesets, quiet=quiet)
    }
    else if (is(genesets, "gsets") | is(genesets, "rgsets")) {
        gsets.obj <- genesets
    } 
    else {
        stop("Genesets must be gsets/rgsets object or named list of genesets")
    }

    # Handling a background population
    if (is(background, "character") & is(background, "vector")) {
        gsets.obj <- gsets.obj$reduce(background)
        background <- length(background)
    }

    # Save gsets object
    args$genesets <- gsets.obj

    # In case results are empty
    plots <- ggempty()

    # Overrepresentation test
    if (test == "hypergeometric") {
        data <- data.frame(matrix(ncol=8, nrow=0))
        colnames(data) <- c("label", "pval", "fdr", "signature", "geneset", "overlap", "background", "hits")
        results <- .hyper_enrichment(signature=signature, 
                                     genesets=gsets.obj$genesets, 
                                     background=background, 
                                     plotting=plotting)
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
        data <- data.frame(matrix(ncol=7, nrow=0))
        colnames(data) <- c("label", "pval", "fdr", "signature", "geneset", "overlap", "score")
        results <- .ks_enrichment(signature=signature, 
                                  genesets=gsets.obj$genesets,
                                  weights=weights, 
                                  weights.pwr=power, 
                                  absolute=absolute, 
                                  plotting=plotting)
    }

    # Ensure signature is present across genesets
    overlap <- .check_overlap(signature, gsets.obj$genesets)

    if (!quiet) {
        cat(.format_str("Signature size: {1}\n", length(signature)))
        cat(.format_str("Number of genesets: {1}\n", length(gsets.obj$genesets)))
        cat(.format_str("Percentage of signature found across genesets: {1}% \n", overlap))
        cat(.format_str("Background population size: {1} \n", background))
        cat(.format_str("P-Value cutoff: {1} \n", pval))
        cat(.format_str("FDR cutoff: {1}\n", fdr))
    }

    # Filter data
    if (!is.null(results$data)) {
        
        data <- results$data %>%
                dplyr::filter(pval <= args$pval) %>%
                dplyr::filter(fdr <= args$fdr) %>%
                dplyr::arrange(pval)

        plots <- results$plots[data$label]
    }
    
    # Wrap dataframe in hyp object
    return(hyp$new(data=data, plots=plots, args=args))
}
