#' Plot top enriched pathways
#'
#' @param df A dataframe
#' @param title Plot title
#' @param top Limit number of pathways shown
#' @param val Choose significance value e.g. c("fdr", "pval")
#' @return A plotly object
#'
#' @importFrom plotly plot_ly plotly_empty add_trace add_annotations layout %>%
#' @keywords internal
.enrichment_plot <- function(df, 
                             title, 
                             top=10, 
                             val=c("fdr", "pval")) {

    # Top pathways
    df <- head(df, top)

    # Handle empty dataframes
    if (nrow(df) == 0) {
        return(plotly_empty())
    }

    # Subset data based on significance value
    if (val == "pval") {
        df_1 <- df[,c(7,1,5,3)]
        val_pretty <- "P-Value"
    } else if (val == "fdr") {
        df_1 <- df[,c(7,2,5,3)]
        val_pretty <- "FDR"
    }

    # Calculate bar heights
    colnames(df_1) <- c("y", "x", "x1", "x2")
    df_2 <- df_1
    df_2$x <- -log10(df_1$x) # Total bar height
    df_2$x2 <- df_1$x2/df_1$x1*df_2$x # First bar height
    df_2$x1 <- df_2$x-df_2$x2 # Second bar height
    y <- factor(df_2$y, levels=df_2$y) # Force order of rownames

    p <- plot_ly(df_2,
                 x = ~x2,
                 y = ~y,
                 type = 'bar',
                 orientation = 'h',
                 hoverinfo = 'y',
                 marker = list(color = '#4CA1AF',
                               line = list(color = 'white',
                                           width = 1))) %>%

                 # Split bars
                 add_trace(x = ~x1, marker = list(color = '#C4E0E5')) %>%

                 # Plot settings
                 layout(title = title,
                        xaxis = list(title = paste("-log<sub>10</sub>(",
                                                   val_pretty,
                                                   ")",
                                                   sep=""),
                                     tickvals=c(-log10(0.05),
                                                -log10(0.01),
                                                -log10(0.001),
                                                seq(5, 1000, 5)),
                                     ticktext=c("-log(0.05)",
                                                "-log(0.01)",
                                                "-log(0.001)"),
                                     tickfont=list(size=9),
                                     showgrid = TRUE,
                                     showline = TRUE,
                                     showticklabels = TRUE,
                                     zeroline = TRUE,
                                     domain = c(0.16, 1)),
                        yaxis = list(title = "",
                                     categoryarray = rev(y),
                                     categoryorder = 'array',
                                     showgrid = TRUE,
                                     showline = TRUE,
                                     showticklabels = FALSE,
                                     zeroline = TRUE),
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
                                             size = 10,
                                             color = 'black'),
                                 showarrow = FALSE,
                                 align = 'right')
    return(p)
}

#'  Visualize hyp/multihyp objects as a bar plot
#'
#' @param hyp_obj A hyp or multihyp object
#' @param title Plot title
#' @param top Limit number of pathways shown
#' @param val Choose significance value e.g. c("fdr", "pval")
#' @param multihyp_titles Use false to disable plot titles for multihyp objects
#' @param show_plots An option to show plots
#' @param return_plots An option to return plots
#' @return A plotly object
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
#' hyp_obj <- hypeR(symbols, REACTOME, bg=2522, fdr=0.05)
#'
#' # Visualize
#' hyp_plot(hyp_obj, top=3, val="fdr")
#'
#' @importFrom stats setNames
#' @importFrom plotly plot_ly add_trace add_annotations layout %>%
#'
#' @export
hyp_plot <- function(hyp_obj, 
                     title="", 
                     top=10, 
                     val=c("fdr", "pval"), 
                     multihyp_titles=TRUE, 
                     show_plots=TRUE, 
                     return_plots=FALSE) {

    stopifnot(is(hyp_obj, "hyp") | is(hyp_obj, "multihyp"))

    # Default arguments
    val <- match.arg(val)

    # Handling of multiple signatures
    if (is(hyp_obj, "multihyp")) {
        multihyp_obj <- hyp_obj
        n <- names(multihyp_obj$data)
        res <- lapply(stats::setNames(n, n), function(x) {
                   hyp_obj <- multihyp_obj$data[[x]]
                   hyp_plot(hyp_obj,
                            ifelse(multihyp_titles, x, ""),
                            top,
                            val,
                            multihyp_titles,
                            show_plots,
                            return_plots)
               })
    } else  {
        df <- hyp_obj$data
        res <- .enrichment_plot(df, title, top, val)
        if (show_plots) {
            show(res)
        }
    }
    if (return_plots) {
        return(res)
    }
}
