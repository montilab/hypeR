#' Plot hiearchy map
#'
#' @param hyp_df A dataframe from a hyp object
#' @param rgsets_obj A relatonal geneset from a hyp object
#' @param title Plot title
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param val Choose significance value displayed when hovering nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @return A visNetwork object
#'
#' @importFrom purrr when
#' @importFrom dplyr filter
#' @importFrom visNetwork visNetwork visNodes visEdges visOptions visInteraction
#' @keywords internal
.hiearchy_map <- function(hyp_df,
                          rgsets_obj,
                          title="", 
                          pval_cutoff=1, 
                          fdr_cutoff=1,
                          val=c("fdr", "pval"),
                          top=NULL) {

    # Subset results
    hyp_df <- hyp_df %>%
              dplyr::filter(pval <= pval_cutoff) %>%
              dplyr::filter(fdr <= fdr_cutoff) %>%
              purrr::when(!is.null(top) ~ head(., top), ~ .)
        
    # Handle empty dataframes
    if (nrow(hyp_df) == 0) {
        return(ggempty())
    }
    
    # Subset relational genesets
    rgsets.obj.subset <- rgsets_obj$subset(hyp_df$label)
    
    # Extract hiearchy information
    gsets <- rgsets.obj.subset$gsets
    nodes <- rgsets.obj.subset$nodes
    edges <- rgsets.obj.subset$edges

    # Node sizes set by geneset length
    size.scaler <- function(x) (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))*30
    nodes$size <- sapply(size.scaler(nodes$length), function(x) ifelse(x < 15, 15, x))
    
    # Node weights based on significance
    weight.scaler <- function(x) (x-max(x, na.rm=TRUE))/(min(x, na.rm=TRUE)-max(x, na.rm=TRUE))
    node.weights <- sapply(nodes$label, function(x) {
                           if (x %in% hyp_df$label) {
                               hyp_df %>%
                               dplyr::filter(label == x) %>%
                               dplyr::pull(val)
                           } else { NA }
                    })

    val.pretty <- ifelse(val == "fdr", "FDR", "P-Value")
    nodes$title <- sapply(node.weights, function(x) {
                       ifelse(is.na(x), NA, paste(val.pretty, x, sep=": "))
                   })

    nodes$color.background <- ifelse(is.na(node.weights), "rgba(88,24,69,1.0)", "rgba(199,0,57,1.0)")
    nodes$color.highlight <- ifelse(is.na(node.weights), "rgba(88,24,69,0.9)", "rgba(199,0,57,0.9)")
    nodes$color.border <- sapply(weight.scaler(node.weights), function(x) { 
                                  if (is.na(x)) {
                                      return("rgb(88,24,69)")
                                  } else{
                                      return(paste("rgba(255,195,0,", round(x, 3), ")", sep=""))   
                                  }
                          })

    visNetwork(nodes, edges, main=list(text=title, style="font-family:Helvetica")) %>%
    visNodes(borderWidth=3, borderWidthSelected=0) %>%
    visEdges(arrows='to', selectionWidth=0) %>%
    visInteraction(multiselect=TRUE, tooltipDelay=300) %>%
    visOptions(highlightNearest=TRUE, collapse=list(enabled=TRUE, clusterOptions=list(color="rgba(199,0,57,1.0)")))
}

#' Visualize hyp/multihyp objects as a hiearchy map
#'
#' @param hyp_obj A hyp or multihyp object
#' @param title Plot title
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param val Choose significance value displayed when hovering nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @param multihyp_titles Use false to disable plot titles for multihyp objects
#' @param show_plots Use true to show plots
#' @param return_plots Use true to return plots
#' @return A visNetwork object or list of visNetwork objects
#'
#' @examples
#' rgsets <- hyperdb_fetch(type="rgsets", "KEGG")
#'
#' signature <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'                "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'                "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' # Perform hyper enrichment
#' hyp_obj <- hypeR(signature, rgsets, bg=2522, fdr_cutoff=0.05)
#'
#' # Visualize
#' hyp_hmap(hyp_obj, top=60)
#'
#' @importFrom stats setNames
#' @importFrom rlang duplicate
#' @export
hyp_hmap <- function(hyp_obj,
                     title="",
                     pval_cutoff=1, 
                     fdr_cutoff=1,
                     val=c("fdr", "pval"),
                     top=NULL,
                     multihyp_titles=TRUE,
                     show_plots=TRUE,
                     return_plots=FALSE) {
    
    # Checks and warnings
    stopifnot(is(hyp_obj, "hyp") | is(hyp_obj, "multihyp"))
    
    # Default arguments
    val <- match.arg(val)

    # Handling multihyp objects
    if (is(hyp_obj, "multihyp")) {
        multihyp_obj <- hyp_obj
        n <- names(multihyp_obj$data)
        res <- lapply(stats::setNames(n, n), function(x) {
                   hyp_obj <- multihyp_obj$data[[x]]
                   hyp_hmap(hyp_obj,
                            ifelse(multihyp_titles, x, ""),
                            pval_cutoff, 
                            fdr_cutoff,
                            val,
                            top,
                            multihyp_titles,
                            show_plots,
                            return_plots)           
               })
    # Handling hyp objects
    } else {
        hyp_df <- hyp_obj$data
        rgsets_obj <- rlang::duplicate(hyp_obj$args$gsets)
        stopifnot(is(rgsets_obj, "rgsets"))
        res <- .hiearchy_map(hyp_df,
                             rgsets_obj,
                             title,
                             pval_cutoff, 
                             fdr_cutoff,
                             val,
                             top)
        if (show_plots) {
            show(res)
        }
    }
    if (return_plots) {
        return(res)
    }
}
