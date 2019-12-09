#' A relational genesets object
#'
#' @name rgsets
#'
#' @section Arguments:
#' \describe{
#'   \item{genesets}{A list of genesets where list names refers to geneset
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
#'   \item{name}{A character vector describing source of genesets}
#'   \item{version}{A character vector describing versioning}
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
#' \code{gsets}
#' \code{pvector}
#'
#' @examples
#' testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
#' rgsets <- rgsets$new(genesets=testdat$genesets, nodes=testdat$nodes, edges=testdat$edges, name="Example", version="v1.0")
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr filter pull %>%
#' 
#' @export
rgsets <- R6Class("rgsets", list(
    genesets = NULL,
    nodes = NULL,
    edges = NULL,
    name = NULL,
    version = NULL,
    initialize = function(genesets, nodes, edges, name="", version="", quiet=FALSE) {
        if (name == "" & !quiet) warning("Describing genesets with a name will aid reproducibility")
        if (version == "" & !quiet) warning("Including a version number will aid reproducibility")
        self$genesets <- genesets
        self$nodes <- nodes
        self$edges <- edges
        self$nodes$id <- rownames(self$nodes)
        self$nodes$length <- sapply(self$nodes$id, function(x) {length(.find_members(x, genesets, nodes, edges))})
        self$name <- name
        self$version <- version
    },
    print = function(...) {
        if (self$name != "") cat(.format_str("{1} ", self$name))
        if (self$version != "") cat(.format_str("{1}", self$version))
        if (self$name != "" | self$version != "") cat("\n\n")
        cat("Genesets\n\n")
        for (i in head(names(self$genesets))) {
            cat(.format_str("{1} ({2})\n", i, length(self$genesets[[i]])))
        }
        cat("\nNodes\n\n")
        base::print(head(self$nodes))
        cat("\nEdges\n\n")
        base::print(head(self$edges)) 
        invisible(self)
    },
    reduce = function(background) {
        genesets <- lapply(self$genesets, function(x) intersect(x, background))
        return(rgsets$new(genesets, self$nodes, self$edges, self$name, self$version, quiet=TRUE))
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
        genesets <- self$genesets[names(self$genesets) %in% labels]
        nodes <- self$nodes[ids,,drop=F]
        edges <- self$edges[self$edges$from %in% ids & self$edges$to %in% ids,,drop=F]
        return(rgsets$new(genesets, nodes, edges, self$name, self$version, quiet=TRUE))
    }
))
