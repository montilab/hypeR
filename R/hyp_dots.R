#' Plot top enriched genesets
#'
#' @param hyp_df A dataframe from a hyp object
#' @param top Limit number of genesets shown
#' @param abrv Abbreviation length of genesetlabels
#' @param sizes Size dots by geneset sizes
#' @param val Choose significance value e.g. c("fdr", "pval")
#' @param title Plot title
#' @return A ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_point labs scale_color_continuous coord_flip geom_hline guides theme element_text element_blank
#' @keywords internal
.dots_plot <- function(hyp_df,
                       top=20,
                       abrv=50,
                       sizes=TRUE,
                       val=c("fdr", "pval"),
                       title="") {
    
    # Default arguments
    val <- match.arg(val)
    
    # Top genesets
    df <- head(hyp_df, top)

    # Handle empty dataframes
    if (nrow(df) == 0) return(ggempty())

    # Plotting variables
    df$color <- df[,val]
    df$size <- if(sizes) df$gset.size else 1

    # Order by significance value
    df <- df[order(-df[,val]),]
    df$label.abrv <- substr(df$label, 1, abrv)
    df$label.abrv <- factor(df$label.abrv, levels=df$label.abrv)

    ggplot(df, aes(x=label.abrv, y=-log10(color), color=color, size=log10(size))) +
    geom_point() +
    labs(title=title, y=ifelse(val == "pval", "-log(P-Value)", "-log(FDR)")) +  
    scale_color_continuous(low="blue", high="red") +
    coord_flip() +
    geom_hline(yintercept=-log10(0.05), linetype="dotted") +
    guides(size=FALSE) + 
    theme(plot.title=element_text(hjust=0.5),
          axis.title.y = element_blank())
}

#'  Visualize hyp/multihyp objects as a dots plot
#'
#' @param hyp_obj A hyp or multihyp object
#' @param top Limit number of genesets shown
#' @param abrv Abbreviation length of geneset labels
#' @param sizes Size dots by geneset sizes
#' @param val Choose significance value e.g. c("fdr", "pval")
#' @param title Plot title
#' @param multihyp_titles Use false to disable plot titles for multihyp objects
#' @param show_plots An option to show plots
#' @param return_plots An option to return plots
#' @return A ggplot object
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
#' # Visualize
#' hyp_dots(hyp_obj, top=3, val="fdr")
#'
#' @importFrom stats setNames
#' @export
hyp_dots <- function(hyp_obj,
                     top=20,
                     abrv=50,
                     sizes=TRUE,
                     val=c("fdr", "pval"), 
                     title="",
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
                   hyp_dots(hyp_obj,
                            top,
                            abrv,
                            sizes,
                            val,
                            ifelse(multihyp_titles, x, ""),
                            multihyp_titles,
                            show_plots,
                            return_plots)
               })
    } else  {
        hyp_df <- hyp_obj$data
        res <- .dots_plot(hyp_df, top, abrv, sizes, val, title)
        if (show_plots) {
            show(res)
        }
    }
    if (return_plots) {
        return(res)
    }
}
