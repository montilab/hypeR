#' @importFrom stats ks.test
#' @keywords internal
.kstest <- function(n.x,
                    y, 
                    weights=NULL,
                    weights.p=1,
                    absolute=FALSE, 
                    do.plot=FALSE, 
                    plot.title="") {
    
    n.y <- length(y)
    if ( n.y < 1 )  stop("Not enough y data")
    if ( any(y > n.x) ) stop( "y must be <= n.x: ", max(y) )
    if ( any(y < 1) ) stop( "y must be positive: ", min(y) )
        
    x.axis <- y.axis <- NULL
    
    if (!is.null(weights)) {
        weights <- abs(weights[y])^weights.p
    
        Pmis <- rep(1, n.x); Pmis[y] <- 0; Pmis <- cumsum(Pmis); Pmis <- Pmis/(n.x-n.y)
        Phit <- rep(0, n.x); Phit[y] <- weights; Phit <- cumsum(Phit); Phit <- Phit/Phit[n.x]
        z <- Phit-Pmis
    
        score <- if (absolute) max(z)-min(z) else z[which.max(abs(z))]
        
        x.axis <- 1:n.x
        y.axis <- z

    } else {
        y <- sort(y)
        n <- n.x * n.y / (n.x + n.y)
        hit <- 1/n.y
        mis <- 1/n.x
        
        Y <- sort(c(y-1, y))
        Y <- Y[diff(Y) != 0]
        y.match <- match(y, Y)
        D <- rep(0, length(Y))
        D[y.match] <- (1:n.y)
        zero <- which(D == 0)[-1]
        D[zero] <- D[zero-1]
        
        z <- D*hit - Y*mis
        
        score <- if (absolute) max(z)-min(z) else z[which.max(abs(z))]
        
        x.axis <- Y
        y.axis <- z
        
        if ( Y[1] > 0 ) {
            x.axis <- c(0, x.axis)
            y.axis <- c(0, y.axis)
        }
        if ( max(Y) < n.x ) {
            x.axis <- c(x.axis, n.x)
            y.axis <- c(y.axis, 0)
        }    
    }

    results <- suppressWarnings(ks.test(1:n.x, y, alternative="less"))
    
    if (do.plot) {
        p <- ggeplot(n.x, y, x.axis, y.axis, plot.title)
    } else {
        p <- NULL
    }
    
    return(list(score=as.numeric(results$statistic), 
                pval=results$p.value, 
                plot=p))
}

.ks_enrichment <- function(signature,
                           gsets,
                           weights=NULL,
                           weights.p=1,
                           absolute=FALSE,
                           do.plots=TRUE) {
  
    if (!is(gsets, "list")) {
        stop("Error: Expected gsets to be a list of gene sets\n")
    }
    if (!is.null(weights)) {
        stopifnot(length(signature) == length(weights))
    }
    
    results <- lapply(seq_len(length(gsets)), function(i) {
                    
                    # Calculate ranks
                    gset <- gsets[[i]]
                    ranks <- match(gset, signature)
                    ranks <- ranks[!is.na(ranks)]
                    
                    # Run ks-test
                    results <- .kstest(length(signature), 
                                       ranks,
                                       weights=weights,
                                       weights.p=weights.p,
                                       absolute=absolute, 
                                       do.plot=do.plots,
                                       plot.title=names(gsets)[[i]])
                    
                    results[['gsets.size']] <- length(gset)
                    results[['genes.found']] <- length(ranks)
                    return(results)
                })
    
    names(results) <- names(gsets)
    
    df <- data.frame(do.call(rbind, results))
    df <- df[order(df$pval),]
    df$fdr <- p.adjust(df$pval, method="fdr")
    plots <- setNames(df$plot, rownames(df))
    
    data <- df[,c("score", "pval", "fdr", "gsets.size", "genes.found")]
    
    return(list(data=data, plots=plots))
}
