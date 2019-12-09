#' Plot top enriched genesets
#'
#' @param hyp_df A dataframe from a hyp object
#' @param top Limit number of genesets shown
#' @param abrv Abbreviation length of genesetlabels
#' @param sizes Size dots by geneset sizes
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param val Choose significance value e.g. c("fdr", "pval")
#' @param title Plot title
#' @return A ggplot object
#'
#' @importFrom purrr when
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_point labs scale_color_continuous scale_y_continuous guide_colorbar coord_flip geom_hline guides theme element_text element_blank
#' 
#' @keywords internal
.dots_plot <- function(hyp_df,
                       top=20,
                       abrv=50,
                       sizes=TRUE,
                       pval_cutoff=1, 
                       fdr_cutoff=1,
                       val=c("fdr", "pval"),
                       title="") {
    
    # Default arguments
    val <- match.arg(val)

    # Subset results
    df <- hyp_df %>%
          dplyr::filter(pval <= pval_cutoff) %>%
          dplyr::filter(fdr <= fdr_cutoff) %>%
          purrr::when(!is.null(top) ~ head(., top), ~ .)

    # Handle empty dataframes
    if (nrow(df) == 0) return(ggempty())

    # Plotting variables
    df$significance <- df[,val]
    df$size <- if(sizes) df$geneset else 1

    # Order by significance value
    df <- df[order(-df[,val]),]
    
    # Abbreviate labels
    label.abrv <- substr(df$label, 1, abrv)
    if (any(duplicated(label.abrv))) {
        stop("Non-unique labels after abbreviating")
    } else {
        df$label.abrv <- factor(label.abrv, levels=label.abrv)   
    }

    ggplot(df, aes(x=label.abrv, y=significance, color=significance, size=log10(size))) +
    geom_point() +
    labs(title=title, y=ifelse(val == "pval", "P-Value", "FDR")) +  
    scale_color_continuous(low="#E53935", high="#114357", guide=guide_colorbar(reverse=TRUE)) +
    coord_flip() +
    scale_y_continuous(trans=.reverselog_trans(10)) +
    geom_hline(yintercept=0.05, linetype="dotted") +
    guides(size=FALSE) + 
    theme(plot.title=element_text(hjust=0.5),
          axis.title.y=element_blank())
}

#' Visualize hyp/multihyp objects as a dots plot
#'
#' @param hyp_obj A hyp or multihyp object
#' @param top Limit number of genesets shown
#' @param abrv Abbreviation length of geneset labels
#' @param sizes Size dots by geneset sizes
#' @param pval Filter results to be less than pval cutoff
#' @param fdr Filter results to be less than fdr cutoff
#' @param val Choose significance value for plot e.g. c("fdr", "pval")
#' @param title Plot title
#' @return A ggplot object
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
#' hyp_dots(hyp_obj, val="fdr")
#'
#' @export
hyp_dots <- function(hyp_obj,
                     top=20,
                     abrv=50,
                     sizes=TRUE,
                     pval=1, 
                     fdr=1,
                     val=c("fdr", "pval"), 
                     title="") {

    stopifnot(is(hyp_obj, "hyp") | is(hyp_obj, "multihyp"))

    # Default arguments
    val <- match.arg(val)

    # Handling of multiple signatures
    if (is(hyp_obj, "multihyp")) {
        multihyp_obj <- hyp_obj
        
        mapply(function(hyp_obj, title) {

            hyp_dots(hyp_obj,
                     top=top,
                     abrv=abrv,
                     sizes=sizes,
                     pval=pval,
                     fdr=fdr,
                     val=val,
                     title=title)

        }, multihyp_obj$data, names(multihyp_obj$data), USE.NAMES=TRUE, SIMPLIFY=FALSE)
    } 
    else {
        .dots_plot(hyp_obj$data, top, abrv, sizes, pval, fdr, val, title)
    }
}
