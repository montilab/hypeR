#' Convert a hyp object to an igraph object
#'
#' @param hyp_obj A hyp object
#' @return An igraph object
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
#' ig <- hyp_to_graph(hyp_obj)
#' 
#' @importFrom igraph graph_from_data_frame set_vertex_attr as_ids V E
#' @importFrom rlang duplicate
#' 
#' @export
hyp_to_graph <- function(hyp_obj) {
    
    # Checks and warnings
    stopifnot(is(hyp_obj, "hyp"))
    hyp_df <- hyp_obj$data
    rgsets_obj <- rlang::duplicate(hyp_obj$args$genesets)
    stopifnot(is(rgsets_obj, "rgsets"))
    
    # Extract nodes and edges
    nodes <- rgsets_obj$nodes
    edges <- rgsets_obj$edges
    
    # Create igraph and copy over enrichment data to graph vertices
    ig <- igraph::graph_from_data_frame(edges, directed=TRUE, vertices=rownames(nodes))
    v.id <- igraph::as_ids(igraph::V(ig))
    v.where <- match(v.id, rownames(nodes))
    v.label <- nodes[v.where, "label"]
    igraph::V(ig)$label <- v.label
    hyp.where <- match(v.label, hyp_df$label)
    for (i in setdiff(colnames(hyp_df), "label")) {
        ig <- igraph::set_vertex_attr(ig, name=i, value=hyp_df[hyp.where, i])
    }
    
    return(ig)
}
