#' Plot hiearchy map
#'
#' @param hyp_df A dataframe from a hyp object
#' @param rgsets_obj A relatonal geneset from a hyp object
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param val Choose significance value displayed when hovering nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @param title Plot title
#' @return A visNetwork object
#'
#' @importFrom purrr when
#' @importFrom dplyr filter
#' @importFrom visNetwork visNetwork visNodes visEdges visOptions visInteraction
#' @keywords internal
.hiearchy_map <- function(hyp_df,
                          rgsets_obj,
                          pval_cutoff=1, 
                          fdr_cutoff=1,
                          val=c("fdr", "pval"),
                          top=NULL,
                          title="") {

    # Subset results
    hyp_df <- hyp_df %>%
              dplyr::filter(pval <= pval_cutoff) %>%
              dplyr::filter(fdr <= fdr_cutoff) %>%
              purrr::when(!is.null(top) ~ head(., top), ~ .)
        
    # Handle empty dataframes
    if (nrow(hyp_df) == 0) return(ggempty())
    
    # Subset relational genesets
    rgsets.obj.subset <- rgsets_obj$subset(hyp_df$label)
    
    # Extract hiearchy information
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
#' @param pval Filter results to be less than pval cutoff
#' @param fdr Filter results to be less than fdr cutoff
#' @param val Choose significance value displayed when hovering nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @param title Plot title
#' @return A visNetwork object
#'
#' @examples
#' genesets <- hyperdb_rgsets("REACTOME", "70.0")
#'
#' signature <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'                "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'                "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' hyp_obj <- hypeR(signature, genesets, background=2522)
#'
#' hyp_hmap(hyp_obj, top=60)
#'
#' @importFrom rlang duplicate
#' @export
hyp_hmap <- function(hyp_obj,
                     pval=1, 
                     fdr=1,
                     val=c("fdr", "pval"),
                     top=NULL,
                     title="") {
    
    # Checks and warnings
    stopifnot(is(hyp_obj, "hyp") | is(hyp_obj, "multihyp"))
    
    # Default arguments
    val <- match.arg(val)

    # Handling of multiple signatures
    if (is(hyp_obj, "multihyp")) {
        multihyp_obj <- hyp_obj

        mapply(function(hyp_obj, title) {

            hyp_hmap(hyp_obj,
                     pval=pval,
                     fdr=fdr,
                     val=val,
                     top=top,
                     title=title) 

        }, multihyp_obj$data, names(multihyp_obj$data), USE.NAMES=TRUE, SIMPLIFY=FALSE)
    } 
    else {
        hyp_df <- hyp_obj$data
        rgsets_obj <- rlang::duplicate(hyp_obj$args$genesets)
        stopifnot(is(rgsets_obj, "rgsets"))
        .hiearchy_map(hyp_df, rgsets_obj, pval, fdr, val, top, title)
    }
}
