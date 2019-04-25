#' Relational genes ets
#'
#' @name rgsets
#'
#' @section Arguments:
#' \describe{
#'   \item{gsets}{A list of genesets}
#'   \item{nodes}{A data frame of labeled nodes}
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

        # Find all parent ids of children ids
        ids.subset <- pvector$new()
        for (id.x in children) {
            
            ids.subset$push(id.x)
            
            parents <- pvector$new(
                           self$edges %>%
                           filter(to == id.x) %>%
                           pull(from)   
                       )
            
            while (length(parents$values) > 0) {
                id.y <- parents$pop()
                id.z <- self$edges %>%
                        filter(to == id.y) %>%
                        pull(from)
                
                ids.subset$push(c(id.y, id.z))  
                parents$push(id.z)
            }
        }
        ids <- unique(ids.subset$values)
        self$gsets <- self$gsets[names(self$gsets) %in% labels]
        self$nodes <- self$nodes[ids,,drop=F]
        self$edges <- self$edges[self$edges$from %in% ids & self$edges$to %in% ids,,drop=F]
    }
))
