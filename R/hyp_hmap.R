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
#' @importFrom plotly plotly_empty 
#' @importFrom visNetwork visNetwork visNodes visEdges visOptions visInteraction
#' 
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
        return(plotly_empty())
    }
    
    # Subset relational genesets
    rgsets.obj.subset <- rgsets_obj$subset(hyp_df$category)
    
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
                           if (x %in% hyp_df$category) {
                               hyp_df %>%
                               dplyr::filter(category == x) %>%
                               dplyr::pull(val)
                           } else{ NA }
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
    visOptions(highlightNearest=TRUE, collapse=list(enabled=TRUE, clusterOptions=list(color.border="black")))
}

#' Visualize hyp or multihyp objects as a hiearchy map
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
#' @importFrom stats setNames
#' @importFrom rlang duplicate
#'
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
    stopifnot("hyp" %in% class(hyp_obj) | "multihyp" %in% class(hyp_obj))
    
    # Default arguments
    val <- match.arg(val)

    # Handling multihyp objects
    if ("multihyp" %in% class(hyp_obj)) {
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
        stopifnot("rgsets" %in% class(rgsets_obj))
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
