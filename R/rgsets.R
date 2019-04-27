#' Find geneset members
#'
#' @importFrom dplyr filter pull %>%
#' @keywords internal
find.members <- function(id, gsets, nodes, edges) {
    label <- nodes[id, "label"]
    if (label %in% names(gsets)) {
        gsets[[label]]
    } else {
        edges %>%
        dplyr::filter(from == id) %>%
        dplyr::pull(to) %>%
        lapply(find.members, gsets, nodes, edges) %>%
        unlist() %>%
        unique()
    }
}

#' Relational genesets
#'
#' @name rgsets
#'
#' @section Arguments:
#' \describe{
#'   \item{gsets}{A list of genesets where list names refers to geneset
#'   labels and values are geneset members represented as a vector}
#'   \item{nodes}{A data frame of labeled nodes e.g.
#'   \cr
#' \tabular{rrrr}{
#'   \tab label\cr
#'   id.1 \tab Geneset Label 1 \tab\cr
#'   id.2 \tab Geneset Label 2 \tab\cr
#'   id.3 \tab Geneset Label 3 \tab\cr
#' }
#'   }
#'   \item{edges}{A data frame of directed edges}
#' }
#'
#' @section Methods:
#' 
#' @section See Also:
#' 
#' \code{pvector}
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr filter pull %>%
#'
#' @export
rgsets <- R6Class("rgsets", list(
    gsets = NULL,
    nodes = NULL,
    edges = NULL,
    initialize = function(gsets, nodes, edges) {
        self$gsets <- gsets
        self$nodes <- nodes
        self$edges <- edges
        self$nodes$id <- rownames(self$nodes)
        self$nodes$length <- sapply(self$nodes$id, function(x) {length(find.members(x, gsets, nodes, edges))})
    },
    print = function(...) {
        cat("gsets\n\n")
        base::print(head(names(self$gsets)))
        cat("\nnodes\n")
        base::print(head(self$nodes))
        cat("\nedges\n")
        base::print(head(self$edges)) 
        invisible(self)
    },
    subset = function(labels) {

        children <- self$nodes %>%
                    subset(label %in% labels) %>%
                    rownames()

        ids.subset <- pvector$new()
        for (id.x in children) {
            
            ids.subset$push(id.x)
            
            parents <- pvector$new(
                           self$edges %>%
                           dplyr::filter(to == id.x) %>%
                           dplyr::pull(from)   
                       )
            
            while (parents$length() > 0) {
                id.y <- parents$pop()
                id.z <- self$edges %>%
                        dplyr::filter(to == id.y) %>%
                        dplyr::pull(from)
                
                ids.subset$push(c(id.y, id.z))  
                parents$push(id.z)
            }
        }
        ids <- unique(ids.subset$values)
        gsets <- self$gsets[names(self$gsets) %in% labels]
        nodes <- self$nodes[ids,,drop=F]
        edges <- self$edges[self$edges$from %in% ids & self$edges$to %in% ids,,drop=F]
        return(rgsets$new(gsets, nodes, edges))
    }
))
