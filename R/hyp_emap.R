#' Plot enrichment map
#'
#' @param hyp_df A dataframe from a hyp object
#' @param genesets A list of genesets
#' @param similarity_metric Metric to calculate geneset similarity
#' @param similarity_cutoff Geneset similarity cutoff
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param val Choose significance value shown above nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @param title Plot title
#' @return A visNetwork object
#'
#' @importFrom purrr when
#' @importFrom dplyr filter
#' @importFrom igraph graph.adjacency V
#' @importFrom visNetwork visNetwork visNodes visEdges visOptions visInteraction toVisNetworkData visIgraphLayout
#' @keywords internal
.enrichment_map <- function(hyp_df,
                            genesets, 
                            similarity_metric=c("jaccard_similarity", "overlap_similarity"),
                            similarity_cutoff=0.2,
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

    # Geneset similarity matrix
    hyp.genesets <- genesets[hyp_df$label]
    hyp.genesets.mat <- sapply(hyp.genesets, function(x) {
        sapply(hyp.genesets, function(y,x) {
            if (similarity_metric == "jaccard_similarity") .jaccard_similarity(x, y)
            else if (similarity_metric == "overlap_similarity") .overlap_similarity(x, y)     
            else stop(.format_str("{1} is an invalid metric", similarity_metric))
        }, x)
    })
    
    m <- as.matrix(hyp.genesets.mat)

    # Sparsity settings
    m[m < similarity_cutoff] <- 0
    
    # Similarity matrix to weighted network
    inet <- igraph::graph.adjacency(m, mode="undirected", weighted=TRUE, diag=FALSE)
    
    # igraph to visnet
    vnet <- toVisNetworkData(inet)

    nodes <- vnet$nodes
    edges <- vnet$edges
    
    # Add edge weights
    edges$value <- vnet$edges$weight

    # Add node scaled sizes based on genset size
    size.scaler <- function(x) (x-min(x))/(max(x)-min(x))*30 
    node.sizes <- sapply(igraph::V(inet), function(x) hyp_df[x, "geneset"])
    nodes$size <-  size.scaler(node.sizes)+20
    
    val.pretty <- ifelse(val == "fdr", "FDR", "P-Value")
    nodes$title <- sapply(igraph::V(inet), function(x) {
                            paste(val.pretty, hyp_df[x, val], sep=": ")
                   })
    
    # Add node scaled weights based on significance
    weight.scaler <- function(x) (x-max(x))/(min(x)-max(x))
    node.weights <- sapply(igraph::V(inet), function(x) hyp_df[x, val])
    nodes$color.border <- "rgb(0,0,0)"
    nodes$color.highlight <- "rgba(199,0,57,0.9)"
    nodes$color.background <- sapply(weight.scaler(node.weights), function(x) { 
                                  if (is.na(x)) {
                                      return("rgba(199,0,57,0)")
                                  } else{
                                      return(paste("rgba(199,0,57,", round(x, 3), ")", sep=""))   
                                  }
                          })

    visNetwork(nodes, edges, main=list(text=title, style="font-family:Helvetica")) %>%
    visNodes(borderWidth=1, borderWidthSelected=0) %>%
    visEdges(color="rgb(88,24,69)") %>%
    visOptions(highlightNearest=TRUE) %>%
    visInteraction(multiselect=TRUE, tooltipDelay=300) %>%
    visIgraphLayout(layout="layout_nicely")
}

#' Visualize hyp/multihyp objects as an enrichment map
#'
#' @param hyp_obj A hyp or multihyp object
#' @param similarity_metric Metric to calculate geneset similarity
#' @param similarity_cutoff Geneset similarity cutoff
#' @param pval Filter results to be less than pval cutoff
#' @param fdr Filter results to be less than fdr cutoff
#' @param val Choose significance value shown above nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @param title Plot title
#' @return A visNetwork object
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
#' hyp_emap(hyp_obj, top=30, val="fdr")
#'
#' @export
hyp_emap <- function(hyp_obj, 
                     similarity_metric=c("jaccard_similarity", "overlap_similarity"),
                     similarity_cutoff=0.2,
                     pval=1, 
                     fdr=1,
                     val=c("fdr", "pval"),
                     top=NULL,
                     title="") {

    stopifnot(is(hyp_obj, "hyp") | is(hyp_obj, "multihyp"))

    # Default arguments
    similarity_metric <- match.arg(similarity_metric)
    val <- match.arg(val)

    # Handling of multiple signatures
    if (is(hyp_obj, "multihyp")) {
        multihyp_obj <- hyp_obj

        mapply(function(hyp_obj, title) {

            hyp_emap(hyp_obj,
                     similarity_metric=similarity_metric, 
                     similarity_cutoff=similarity_cutoff,
                     pval=pval,
                     fdr=fdr,
                     val=val,
                     top=top,
                     title=title) 

        }, multihyp_obj$data, names(multihyp_obj$data), USE.NAMES=TRUE, SIMPLIFY=FALSE)
    } 
    else {
        hyp_df <- hyp_obj$data
        genesets <- hyp_obj$args$genesets$genesets
        .enrichment_map(hyp_df, genesets, similarity_metric, similarity_cutoff, pval, fdr, val, top, title)
    }
}
