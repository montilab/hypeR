#' Find geneset members
#'
#' @param id A vector of ids
#' @param gsets A list of genesets (see \code{rgsets})
#' @param nodes A data frame of labeled nodes (see \code{rgsets})
#' @param edges A data frame of directed edges (see \code{rgsets})
#' @return A vector of ids
#'
#' @importFrom dplyr filter pull %>%
#' @keywords internal
find_members <- function(id, gsets, nodes, edges) {
    label <- nodes[id, "label"]
    if (label %in% names(gsets)) {
        gsets[[label]]
    } else {
        edges %>%
        dplyr::filter(from == id) %>%
        dplyr::pull(to) %>%
        lapply(find_members, gsets, nodes, edges) %>%
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
#'     \cr
#'     \tabular{rrrr}{
#'       \tab label\cr
#'       G1 \tab Geneset 1 \tab\cr
#'       G2 \tab Geneset 2 \tab\cr
#'       G3 \tab Geneset 3 \tab\cr
#'     }
#'   }
#'   \item{edges}{A data frame of directed edges
#'     \cr
#'     \tabular{rrrr}{
#'       from \tab to\cr
#'       G1 \tab G2 \tab\cr
#'       G1 \tab G3 \tab\cr
#'     }
#'   }
#' }
#'
#' @section Methods:
#'
#' \code{print(rgsets)} shows some information about the object data
#'
#' \code{rgsets$subset(labels)} returns an rgsets object subsetted
#' on geneset labels
#' 
#' @section See Also:
#' 
#' \code{pvector}
#'
#' @examples
#' testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
#' rgsets <- rgsets$new(gsets=testdat$gsets,
#'                      nodes=testdat$nodes,
#'                      edges=testdat$edges)
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr filter pull %>%
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
        self$nodes$length <- sapply(self$nodes$id, function(x) {length(find_members(x, gsets, nodes, edges))})
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
