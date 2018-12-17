#' @export
db.info <- function() {
    cat("Downloaded from: http://software.broadinstitute.org/gsea/msigdb\n")
    cat("HM | Hallmark gene sets, gene symbols\n")
    cat("C1 | Positional gene sets, gene symbols\n")
    cat("C2 | All Curated gene sets, gene symbols\n")
    cat("C3 | All Motif gene sets, gene symbols\n")
    cat("C4 | All Computational gene sets, gene symbols\n")
    cat("C5 | All Go gene sets, gene symbols\n")
    cat("C6 | All oncogenic signatures gene sets, gene symbols\n")
    cat("C7 | All immunologic signatures gene sets, gene symbols\n")
}

#' @export
db.get <- function(symbol) {
    db.path <- list()
    db.path[["HM"]] <- "h.all.v6.2.symbols.gmt"
    db.path[["C1"]] <- "c1.all.v6.2.symbols.gmt" 
    db.path[["C2"]] <- "c2.all.v6.2.symbols.gmt" 
    db.path[["C3"]] <- "c3.all.v6.2.symbols.gmt" 
    db.path[["C4"]] <- "c4.all.v6.2.symbols.gmt" 
    db.path[["C5"]] <- "c5.all.v6.2.symbols.gmt" 
    db.path[["C6"]] <- "c6.all.v6.2.symbols.gmt" 
    db.path[["C7"]] <- "c7.all.v6.2.symbols.gmt" 
    
    # Load database
    db <- system.file("extdata", db.path[[symbol]], package = "hypeR")
    gs <- getGeneSet(new("GeneSet", db))
    return(gs)
}

#' @export
hypeR <- function(symbols,
                  gsets,
                  bg=23467, 
                  min.drawsize=4,
                  mht=TRUE,    
                  fdr=1,
                  verbose=FALSE) {
    
    cat("Number of genes = ", length(symbols), "\n")
    cat("Number of gene sets = ", length(gsets), "\n")
    cat("Background population = ", bg, "\n")
    cat("FDR cutoff = ", fdr, "\n")
           
    df <- data.frame(matrix(ncol=8, nrow=0))
    colnames(df) <- c("pval","fdr","set.annotated","set.size","category.annotated","total.annotated","category","hits")
    
    hyp <- hyper_enrichment(drawn=symbols, 
                            categories=gsets, 
                            ntotal=bg, 
                            min.drawsize=min.drawsize,
                            mht=mht,
                            verbose=verbose)
    
    # Formatting if hits are found
    if (!is.null(hyp)) {
        df <- data.frame(hyp, stringsAsFactors=F)
        df[,c(1:6)] <- lapply(df[,c(1:6)], as.numeric)
        df <- df[complete.cases(df),,drop=F]
        df <- df[df$fdr <= fdr,,drop=F]
    }
    return(df)
}