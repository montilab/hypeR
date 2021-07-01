#' Plot hiearchy map
#'
#' @param hyp_df A dataframe from a hyp object
#' @param rgsets_obj A relatonal geneset from a hyp object
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param val Choose significance value displayed when hovering nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @param title Plot title
#' @param graph Return an igraph object instead
#' @return A visNetwork object
#'
#' @importFrom purrr when
#' @importFrom dplyr filter
#' @importFrom igraph graph_from_data_frame set_vertex_attr as_ids V E
#' @importFrom visNetwork visNetwork visNodes visEdges visOptions visInteraction visIgraphLayout
#' 
#' @keywords internal
.hiearchy_map <- function(hyp_df,
                          rgsets_obj,
                          pval_cutoff=1, 
                          fdr_cutoff=1,
                          val=c("fdr", "pval"),
                          top=NULL,
                          title="",
                          graph=FALSE) {

    # Subset results
    hyp_df <- hyp_df %>%
              dplyr::filter(pval <= pval_cutoff) %>%
              dplyr::filter(fdr <= fdr_cutoff) %>%
              purrr::when(!is.null(top) ~ head(., top), ~ .)
        
    # Handle empty dataframes
    if (nrow(hyp_df) == 0) return(NULL)
    
    # Subset relational genesets
    rgsets.obj.subset <- rgsets_obj$subset(hyp_df$label)
    
    # Extract hiearchy information
    nodes <- rgsets.obj.subset$nodes
    edges <- rgsets.obj.subset$edges

    # Node sizes set by geneset length
    size.scaler <- function(x) (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))*30
    nodes$size <- sapply(size.scaler(nodes$length), function(x) ifelse(x < 15, 15, x))
    
    # Node weights based on significance
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

    nodes$color.background <- ifelse(is.na(node.weights), "#581845", "#C70039")
    nodes$color.highlight <- ifelse(is.na(node.weights), .hexa("#581845", 0.9), .hexa("#C70039", 0.9))
    weight.scaler <- function(x) (x-max(x, na.rm=TRUE))/(min(x, na.rm=TRUE)-max(x, na.rm=TRUE))
    nodes$color.border <- sapply(weight.scaler(node.weights), function(x) { 
                                  if (is.na(x)) {
                                      return("#581845")
                                  } else{
                                      return(.hexa("#FFC300", x))
                                  }
                          })

    if (graph) {
        ig <- igraph::graph_from_data_frame(edges, directed=TRUE, vertices=rownames(nodes))
        v.id <- igraph::as_ids(igraph::V(ig))
        v.where <- match(v.id, rownames(nodes))
        v.label <- nodes[v.where, "label"]
        v.weight <- sapply(v.label, function(x) {
            if (x %in% hyp_df$label) {
                hyp_df %>%
                dplyr::filter(label == x) %>%
                dplyr::pull(val)
            } else { NA }
        })
        ig <- igraph::set_vertex_attr(ig, name=val, value=v.weight)
        igraph::V(ig)$size = nodes[v.where, "size"]
        igraph::V(ig)$color = nodes[v.where, "color.background"]
        igraph::V(ig)$frame.color = nodes[v.where, "color.border"]
        igraph::V(ig)$label = v.label
        igraph::V(ig)$type = ifelse(is.na(v.weight), "internal", "leaf")
        igraph::V(ig)$label.family = "Helvetica"
        igraph::V(ig)$label.color = "#000000"
        igraph::V(ig)$label.font = 2
        igraph::V(ig)$label.cex = 0.75
        igraph::V(ig)$label.dist = 1
        igraph::E(ig)$width = 1
        igraph::E(ig)$color = "#3D3D3D"
        igraph::E(ig)$arrow.size= 0.2
        return(ig)
    } else {
        visNetwork(nodes, edges, main=list(text=title, style="font-family:Helvetica")) %>%
        visNodes(borderWidth=3, borderWidthSelected=0) %>%
        visEdges(arrows='to', selectionWidth=0) %>%
        visIgraphLayout(layout="layout_nicely") %>%  
        visInteraction(multiselect=TRUE, tooltipDelay=300) %>%
        visOptions(highlightNearest=TRUE, collapse=list(enabled=TRUE, clusterOptions=list(color="rgba(199,0,57,1.0)")))   
    }
}

#' Visualize hyp/multihyp objects as a hiearchy map
#'
#' @param hyp_obj A hyp or multihyp object
#' @param pval Filter results to be less than pval cutoff
#' @param fdr Filter results to be less than fdr cutoff
#' @param val Choose significance value displayed when hovering nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @param title Plot title
#' @param graph Return an igraph object instead
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
#' 
#' @export
hyp_hmap <- function(hyp_obj,
                     pval=1, 
                     fdr=1,
                     val=c("fdr", "pval"),
                     top=NULL,
                     title="",
                     graph=FALSE) {
    
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
                     title=title,
                     graph=graph)

        }, multihyp_obj$data, names(multihyp_obj$data), USE.NAMES=TRUE, SIMPLIFY=FALSE)
    } 
    else {
        hyp_df <- hyp_obj$data
        rgsets_obj <- rlang::duplicate(hyp_obj$args$genesets)
        stopifnot(is(rgsets_obj, "rgsets"))
        .hiearchy_map(hyp_df, rgsets_obj, pval, fdr, val, top, title, graph)
    }
}
