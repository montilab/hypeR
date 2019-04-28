#' Perform hyper enrichment
#'
#' @param symbols A vector of gene symbols
#' @param gsets A list of gene sets
#' @param gsets_relational Use true to inform gsets are relational
#' @param bg Size or character vector of background population genes
#' @param min_drawsize Min number of drawn items that must be among categories items
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param verbose Use false to suppress logs
#' @return A hyp object
#'
#' @examples
#' # Grab a list of curated gene sets
#' gsets <- readRDS(system.file("extdata/gsets.rds", package="hypeR"))
#' REACTOME <- gsets$REACTOME
#'
#' # Genes involed in tricarboxylic acid cycle
#' symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'              "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'              "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' # Perform hyper enrichment
#' hyp_obj <- hypeR(symbols, REACTOME, bg=2522, fdr_cutoff=0.05)
#'
#' @importFrom stats complete.cases
#' @export
hypeR <- function(symbols,
                  gsets,
                  gsets_relational=FALSE,
                  bg=23467,
                  min_drawsize=4,
                  pval_cutoff=1,
                  fdr_cutoff=1,
                  verbose=FALSE) {

    # Save original arguments
    args <- as.list(environment())
    
    # Check if gsets are relational
    if (gsets_relational) {
        stopifnot(is(gsets, "rgsets"))
        gsets <- gsets$gsets
    }

    # Handling of multiple signatures
    if (is(symbols, "list")) {
        lhyp <- lapply(symbols, hypeR, args$gsets, 
                                       args$gsets_relational, 
                                       args$bg, 
                                       args$min_drawsize, 
                                       args$pval_cutoff,
                                       args$fdr_cutoff, 
                                       args$verbose
                )
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
        cat("Number of genes = ", length(symbols), "\n")
        cat("Number of gene sets = ", length(gsets), "\n")
        cat("Background population size = ", bg, "\n")
        cat("P-Value cutoff = ", pval_cutoff, "\n")
        cat("FDR cutoff = ", fdr_cutoff, "\n")
    }

    df <- data.frame(matrix(ncol=8, nrow=0))
    colnames(df) <- c("pval","fdr","set.annotated","set.size","category.annotated","total.annotated","category","hits")

    results <- .hyper_enrichment(drawn=symbols,
                                 categories=gsets,
                                 ntotal=bg,
                                 min.drawsize=min_drawsize,
                                 mht=TRUE,
                                 verbose=verbose)

    # If hits are found format dataframe
    if (!is.null(results)) {
        df <- data.frame(results, stringsAsFactors=FALSE)
        df[,seq_len(6)] <- lapply(df[,seq_len(6)], as.numeric)
        df <- df[stats::complete.cases(df),,drop=FALSE]
        df <- df[df$pval <= pval_cutoff,,drop=FALSE]
        df <- df[df$fdr <= fdr_cutoff,,drop=FALSE]
    }

    # Wrap dataframe in hyp object
    hyp.obj <- hyp$new(data=df, args=args)
    return(hyp.obj)
}
