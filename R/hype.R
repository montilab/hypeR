#' Perform hyper enrichment
#'
#' @param symbols A character vector of gene symbols
#' @param gsets A list of gene sets
#' @param bg Size of background population
#' @param min.drawsize Min number of drawn items that must be among categories items
#' @param pval.cutoff Filter results to be less than pval cutoff
#' @param fdr.cutoff Filter results to be less than fdr cutoff
#' @param verbose Use false to suppress logs
#' @return A hyper dataframe
#'
#' @examples
#' # Grab a list of curated gene sets
#' REACTOME <- db.get("C2.CP.REACTOME")
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

    cat("Number of genes = ", length(symbols), "\n")
    cat("Number of gene sets = ", length(gsets), "\n")
    cat("Background population = ", bg, "\n")
    cat("P-Value cutoff = ", pval.cutoff, "\n")
    cat("FDR cutoff = ", fdr.cutoff, "\n")

    df <- data.frame(matrix(ncol=8, nrow=0))
    colnames(df) <- c("pval","fdr","set.annotated","set.size","category.annotated","total.annotated","category","hits")

    hyp <- hyper.enrichment(drawn=symbols,
                            categories=gsets,
                            ntotal=bg,
                            min.drawsize=min.drawsize,
                            mht=TRUE,
                            verbose=verbose)

    # If hits are found format dataframe
    if (!is.null(hyp)) {
        df <- data.frame(hyp, stringsAsFactors=F)
        df[,c(1:6)] <- lapply(df[,c(1:6)], as.numeric)
        df <- df[complete.cases(df),,drop=F]
        df <- df[df$pval <= pval.cutoff,,drop=F]
        df <- df[df$fdr <= fdr.cutoff,,drop=F]
    }
    return(df)
}

#' Convert hyper dataframe to an interactive datatable
#'
#' @param df A hyper dataframe
#' @param simple Use true to only include essential dataframe columns
#' @param stylish Use true to add a bootstrap styling theme to datatable
#' @return A datatable object
#'
#' @examples
#' # Grab a list of curated gene sets
#' REACTOME <- db.get("C2.CP.REACTOME")
#' 
#' # Genes involed in tricarboxylic acid cycle
#' symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'              "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'              "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#' 
#' # Perform hyper enrichment
#' hyp <- hypeR(symbols, REACTOME, bg=2522, fdr=0.05)
#' 
#' # Export
#' hyp.show(hyp)
#'
#' @import DT
#' @export
hyp.show <- function(df, simple=TRUE, stylish=FALSE) {
    if (simple) {
        cols <- c(1,2,7,8)
    } else {
        cols <- c(1:ncol(df))
    }

    # Gene symbols converted to hyperlinks
    url <- "https://www.genecards.org/cgi-bin/carddisp.pl?gene="
    df$hits <- lapply(df$hits, function(x) {
                   sapply(symbols, function(x) {
                       paste('<a href="',
                             url,
                             x,
                             '">',
                             x,
                             '</a>',
                             sep="")
                   })
               })

    if (stylish) {
        datatable(data = df[,cols,drop=F],
                  style = 'bootstrap',
                  class = 'table-bordered table-condensed',
                  escape = TRUE,
                  rownames = FALSE)
    } else {
        datatable(data = df[,cols,drop=F],
                  escape = TRUE,
                  rownames = FALSE)
    }
}

#' Export hyper dataframe to excel
#'
#' @param df A hyper dataframe
#' @param file.path Output file path
#' @param cols Dataframe columns to include
#' @return None
#'
#' @examples
#' # Grab a list of curated gene sets
#' REACTOME <- db.get("C2.CP.REACTOME")
#' 
#' # Genes involed in tricarboxylic acid cycle
#' symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'              "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'              "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#' 
#' # Perform hyper enrichment
#' hyp <- hypeR(symbols, REACTOME, bg=2522, fdr=0.05)
#' 
#' # Export
#' hyp.to.excel(hyp, file.path="pathways.xlsx")
#'
#' @import openxlsx
#' @export
hyp.to.excel <- function(df, file.path, cols=c(1:ncol(df))) {
    write.xlsx(x = df[,cols,drop=F],
               file = file.path,
               col.names = TRUE,
               row.names = FALSE)
}

#' Export hyper dataframe to table
#'
#' @param df A hyper dataframe
#' @param file.path Output file path
#' @param sep The field separator string
#' @param cols Dataframe columns to include
#' @return None
#'
#' @examples
#' # Grab a list of curated gene sets
#' REACTOME <- db.get("C2.CP.REACTOME")
#' 
#' # Genes involed in tricarboxylic acid cycle
#' symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'              "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'              "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#' 
#' # Perform hyper enrichment
#' hyp <- hypeR(symbols, REACTOME, bg=2522, fdr=0.05)
#' 
#' # Export
#' hyp.to.table(hyp, file.path="pathways.txt")
#'
#' @export
hyp.to.table <- function(df, file.path, sep="\t", cols=c(1:ncol(df))) {
    write.table(x = df[,cols,drop=F],
                file = file.path,
                quote = FALSE,
                sep = sep,
                col.names = TRUE,
                row.names = FALSE)
}

#' Visualize top enriched pathways
#'
#' @param df A hyper dataframe
#' @param top Limit number of pathways shown
#' @param val Choose significance value e.g. c("pval", "fdr")
#' @return A plotly object
#'
#' @examples
#' # Grab a list of curated gene sets
#' REACTOME <- db.get("C2.CP.REACTOME")
#' 
#' # Genes involed in tricarboxylic acid cycle
#' symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'              "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'              "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#' 
#' # Perform hyper enrichment
#' hyp <- hypeR(symbols, REACTOME, bg=2522, fdr=0.05)
#' 
#' # Visualize
#' hyp.plot(hyp, top=3, val="fdr")
#'
#' @import plotly
#' @export
hyp.plot <- function(df, top=10, val=c("fdr", "pval")) {

    # Default arguments
    val <- match.arg(val)

    # Top pathways
    df <- head(df, top)

    # Subset data based on significance value
    if (val == "pval") {
        df.1 <- df[,c(7,1,5,3)]
        val.pretty <- "P-Value"
    } else if (val == "fdr") {
        df.1 <- df[,c(7,2,5,3)]
        val.pretty <- "FDR"
    }

    # Calculate bar heights
    colnames(df.1) <- c("y", "x", "x1", "x2")
    df.2 <- df.1
    df.2$x <- -log10(df.1$x) # Total bar height
    df.2$x2 <- df.1$x2/df.1$x1*df.2$x # Second bar height
    df.2$x1 <- df.2$x-df.2$x2 # First bar height
    y <- factor(df.2$y, levels=df.2$y) # Force order of rownames

    p <- plot_ly(df.2,
                 x = ~x1,
                 y = ~y,
                 type = 'bar',
                 orientation = 'h',
                 hoverinfo = 'y',
                 marker = list(color = '#4CA1AF',
                               line = list(color = 'white',
                                           width = 1))) %>%

                 # Split bars
                 add_trace(x = ~x2, marker = list(color = '#C4E0E5')) %>%

                 # Plot settings
                 layout(title = "Top Pathways",
                        xaxis = list(title = paste("-log<sub>10</sub>(",
                                                   val.pretty,
                                                   ")",
                                                   sep=""),
                                     showgrid = T,
                                     showline = T,
                                     showticklabels = T,
                                     zeroline = T,
                                     domain = c(0.16, 1)),
                        yaxis = list(title = "",
                                     categoryarray = rev(y),
                                     categoryorder = 'array',
                                     showgrid = T,
                                     showline = T,
                                     showticklabels = F,
                                     zeroline = T),
                        barmode = 'stack',
                        paper_bgcolor = 'white',
                        plot_bgcolor = 'white',
                        margin = list(l = 300, r = 0, t = 30, b = 40),
                        showlegend = FALSE) %>%

                 # Labeling the y-axis
                 add_annotations(xref = 'paper',
                                 yref = 'y',
                                 x = 0.15,
                                 y = y,
                                 xanchor = 'right',
                                 text = y,
                                 categoryorder = 'array',
                                 font = list(family = 'Arial',
                                             size = 12,
                                             color = 'black'),
                                 showarrow = FALSE,
                                 align = 'right')
    return(p)
}