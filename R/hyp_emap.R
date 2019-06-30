#' Calculate jaccard similarity of two sets
#'
#' @param a A vector
#' @param b A vector
#' @return A numerical value
#'
#' @keywords internal
jaccard_similarity <- function(a, b) {
    length( intersect(a, b) ) / length( union(a, b) )
}

#' Calculate overlap similarity of two sets
#'
#' @param a A vector
#' @param b A vector
#' @return A numerical value
#'
#' @keywords internal
overlap_similarity <- function(a, b) {
    length( intersect(a, b) ) / min( length(a), length(b) )
}

#' Plot enrichment map
#'
#' @param hyp_df A dataframe from a hyp object
#' @param gsets A list of genesets
#' @param title Plot title
#' @param similarity_metric Metric to calculate geneset similarity
#' @param similarity_cutoff Geneset similarity cutoff
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param val Choose significance value shown above nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @return A visNetwork object
#'
#' @importFrom purrr when
#' @importFrom dplyr filter
#' @importFrom igraph graph.adjacency V
#' @importFrom visNetwork visNetwork visNodes visEdges visOptions visInteraction toVisNetworkData visIgraphLayout
#' @keywords internal
.enrichment_map <- function(hyp_df,
                            gsets, 
                            title, 
                            similarity_metric=c("jaccard_similarity", "overlap_similarity"),
                            similarity_cutoff=0.2,
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

    # Geneset similarity matrix
    hyp.gsets <- gsets[hyp_df$label]
    hyp.gsets.mat <- sapply(hyp.gsets, function(x) {
        sapply(hyp.gsets, function(y,x) {
            if (similarity_metric == "jaccard_similarity") jaccard_similarity(x, y)
            else if (similarity_metric == "overlap_similarity") overlap_similarity(x, y)     
            else stop(paste(similarity_metric, "is an invalid metric"))
        }, x)
    })
    
    m <- as.matrix(hyp.gsets.mat)

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
    node.sizes <- sapply(igraph::V(inet), function(x) hyp_df[x, "gset.size"])
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
#' @param title Plot title
#' @param similarity_metric Metric to calculate geneset similarity
#' @param similarity_cutoff Geneset similarity cutoff
#' @param pval_cutoff Filter results to be less than pval cutoff
#' @param fdr_cutoff Filter results to be less than fdr cutoff
#' @param val Choose significance value shown above nodes e.g. c("fdr", "pval")
#' @param top Limit number of pathways shown
#' @param multihyp_titles Use false to disable plot titles for multihyp objects
#' @param show_plots An option to show plots
#' @param return_plots An option to return plots
#' @return A visNetwork object or list of visNetwork objects
#'
#' @examples
#' gsets <- hyperdb_fetch(type="gsets", "KEGG_2019_Human")
#'
#' signature <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'                "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'                "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' # Perform hyper enrichment
#' hyp_obj <- hypeR(signature, gsets, bg=2522, fdr_cutoff=0.05)
#'
#' # Visualize
#' hyp_emap(hyp_obj, top=30, val="fdr")
#'
#' @importFrom stats setNames
#' @export
hyp_emap <- function(hyp_obj, 
                     title="",
                     similarity_metric=c("jaccard_similarity", "overlap_similarity"),
                     similarity_cutoff=0.2,
                     pval_cutoff=1, 
                     fdr_cutoff=1,
                     val=c("fdr", "pval"),
                     top=NULL,
                     multihyp_titles=TRUE,
                     show_plots=TRUE, 
                     return_plots=FALSE) {

    stopifnot(is(hyp_obj, "hyp") | is(hyp_obj, "multihyp"))

    # Default arguments
    similarity_metric <- match.arg(similarity_metric)
    val <- match.arg(val)

    # Handling of multiple signatures
    if (is(hyp_obj, "multihyp")) {
        multihyp_obj <- hyp_obj
        n <- names(multihyp_obj$data)
        res <- lapply(stats::setNames(n, n), function(x) {
                   hyp_obj <- multihyp_obj$data[[x]]
                   hyp_emap(hyp_obj,
                            ifelse(multihyp_titles, x, ""),
                            similarity_metric, 
                            similarity_cutoff,
                            pval_cutoff,
                            fdr_cutoff,
                            val,
                            top,
                            multihyp_titles,
                            show_plots,
                            return_plots)           
               })
    } else {
        hy_df <- hyp_obj$data
        # Check if gsets are relational
        if (is(hyp_obj$args$gsets, "rgsets")) {
            gsets <- hyp_obj$args$gsets$gsets
        } else {
            gsets <- hyp_obj$args$gsets
        }
        res <- .enrichment_map(hy_df,
                               gsets, 
                               title,
                               similarity_metric,
                               similarity_cutoff,
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
