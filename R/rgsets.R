#' @title A relational genesets object
#' @section See Also:
#' 
#' \code{gsets}
#' 
#' @importFrom R6 R6Class
#' @importFrom dplyr filter pull %>%
#' 
#' @export
rgsets <- R6Class("rgsets", list(
    #' @field genesets A list of genesets where list names refers to geneset labels and values are geneset members represented as a vector
    #' @field nodes A data frame of labeled nodes
    #' @field edges A data frame of directed edges
    #' @field name A character vector describing source of genesets
    #' @field version A character vector describing versioning
    genesets = NULL,
    nodes = NULL,
    edges = NULL,
    name = NULL,
    version = NULL,

    #' @description
    #' Create a rgsets object
    #' @param genesets A list of genesets where list names refers to geneset labels and values are geneset members represented as a vector
    #' @param nodes A data frame of labeled nodes
    #' @param edges A data frame of directed edges
    #' @param name A character vector describing source of genesets
    #' @param version A character vector describing versioning
    #' @param quiet Use true to silence warnings
    #' @return A new rgsets object
    initialize = function(genesets, nodes, edges, name="Custom", version="", quiet=FALSE) {
        # Handle versioning information
        if (name == "Custom" & !quiet) warning("Describing genesets with a name will aid reproducibility")
        if (version == "" & !quiet) warning("Including a version number will aid reproducibility")
        
        # Basic checks
        stopifnot(is(edges, "data.frame"))
        stopifnot(ncol(edges) == 2)
        stopifnot(colnames(edges) == c("from", "to"))
        stopifnot(is(nodes, "data.frame"))
        stopifnot(ncol(nodes) > 0)
        stopifnot("label" %in% colnames(nodes))
        
        if (!all(unique(unlist(edges)) %in% rownames(nodes))) {
            stop("Some edge entities are missing from nodes")
        }
        if (any(is.na(edges$from) | is.na(edges$to))) {
            edges <- edges[complete.cases(edges),]
            warning("Dropping incomplete edges")
        }
        if (any(edges$from == edges$to)) {
            edges <- edges[edges$from != edges$to,]
            warning("Dropping edges with self loops")
        }
        
        self$genesets <- genesets
        self$nodes <- nodes
        self$edges <- edges
        self$nodes$id <- rownames(self$nodes)
        self$nodes$length <- sapply(self$nodes$id, function(x) {length(.find_members(x, genesets, nodes, edges))})
        self$name <- name
        self$version <- version
    },
    #' @description
    #' Print relational genesets information
    #' @return NULL
    print = function() {
        cat(self$info(), "\n\n")
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
    #' @description
    #' Returns versioning information
    #' @return A character vector with name and version
    info = function() {
        return(.format_str("{1} {2}", self$name, self$version))
    },
    #' @description
    #' Reduces genesets to a background distribution of symbols
    #' @param background A character vector of symbols
    #' @return A rgsets object
    reduce = function(background) {
        genesets <- lapply(self$genesets, function(x) intersect(x, background))
        return(rgsets$new(genesets, self$nodes, self$edges, self$name, self$version, quiet=TRUE))
    },
    #' @description
    #' Subsets genesets on a character vector of labels
    #' @param labels A character vector of genesets
    #' @return A rgsets object
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
