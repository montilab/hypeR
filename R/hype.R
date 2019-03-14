#' Perform hyper enrichment
#'
#' @param symbols A character vector of gene symbols
#' @param gsets A list of gene sets
#' @param bg Size or character vector of background population genes
#' @param min.drawsize Min number of drawn items that must be among categories items
#' @param pval.cutoff Filter results to be less than pval cutoff
#' @param fdr.cutoff Filter results to be less than fdr cutoff
#' @param verbose Use false to suppress logs
#' @return A hyper object
#'
#' @examples
#' # Grab a list of curated gene sets
#' REACTOME <- ex_get("C2.CP.REACTOME")
#'
#' # Genes involed in tricarboxylic acid cycle
#' symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'              "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'              "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' # Perform hyper enrichment
#' hyp <- hypeR(symbols, REACTOME, bg=2522, fdr=0.05)
#'
#' @export
hypeR <- function(symbols,
                  gsets,
                  bg=23467,
                  min.drawsize=4,
                  pval.cutoff=1,
                  fdr.cutoff=1,
                  verbose=FALSE) {

    # Handling of multiple signatures
    if (class(symbols) == "list") {
        hyp <- lapply(symbols, hypeR, gsets, bg, min.drawsize, pval.cutoff, fdr.cutoff, verbose)
        return(hyp)
    }
    # Handling a background population
    if ("character" %in% is(bg) & "vector" %in% is(bg)) {
        gsets <- lapply(gsets, function(x) intersect(x, bg))
        bg <- length(bg)
    }

    cat("Number of genes = ", length(symbols), "\n")
    cat("Number of gene sets = ", length(gsets), "\n")
    cat("Background population size = ", bg, "\n")
    cat("P-Value cutoff = ", pval.cutoff, "\n")
    cat("FDR cutoff = ", fdr.cutoff, "\n")

    df <- data.frame(matrix(ncol=8, nrow=0))
    colnames(df) <- c("pval","fdr","set.annotated","set.size","category.annotated","total.annotated","category","hits")

    hyp <- .hyper_enrichment(drawn=symbols,
                             categories=gsets,
                             ntotal=bg,
                             min.drawsize=min.drawsize,
                             mht=TRUE,
                             verbose=verbose)

    # If hits are found format dataframe
    if (!is.null(hyp)) {
        df <- data.frame(hyp, stringsAsFactors=FALSE)
        df[,seq_len(6)] <- lapply(df[,seq_len(6)], as.numeric)
        df <- df[complete.cases(df),,drop=FALSE]
        df <- df[df$pval <= pval.cutoff,,drop=FALSE]
        df <- df[df$fdr <= fdr.cutoff,,drop=FALSE]
    }
    return(df)
}
