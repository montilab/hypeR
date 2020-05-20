#' @title A genesets object
#' 
#' @examples
#' genesets <- list("GSET1" = c("GENE1", "GENE2", "GENE3"),
#'                  "GSET2" = c("GENE4", "GENE5", "GENE6"),
#'                  "GSET3" = c("GENE7", "GENE8", "GENE9"))
#'
#' gsets_obj <- gsets$new(genesets, name="example", version="v1.0")
#' print(gsets_obj)
#' 
#' @section See Also:
#' 
#' \code{rgsets}
#' 
#' @importFrom R6 R6Class
#' 
#' @export
gsets <- R6Class("gsets", list(
    #' @field genesets A named list of genesets
    #' @field name A character vector describing source of genesets
    #' @field version A character vector describing versioning
    genesets = NULL,
    name = NULL,
    version = NULL,

    #' @description
    #' Create a gsets object
    #' @param genesets A named list of genesets
    #' @param name A character vector describing source of genesets
    #' @param version A character vector describing versioning
    #' @param clean Use true to clean labels of genesets
    #' @param quiet Use true to silence warnings
    #' @return A new gsets object
    initialize = function(genesets, name="Custom", version="", clean=FALSE, quiet=FALSE) {
        if (!is(genesets, "list")) stop("Genesets must be a named list of symbols")
        if (is.null(names(genesets))) stop("Genesets must be a named list of symbols")
        
        # Handle versioning information
        if (name == "Custom" & !quiet) warning("Describing genesets with a name will aid reproducibility")
        if (version == "" & !quiet) warning("Including a version number will aid reproducibility")
        
        if (clean) {
            names(genesets) <- clean_genesets(names(genesets))
        }
        
        self$genesets <- genesets
        self$name <- name
        self$version <- version
    },
    #' @description
    #' Print genesets information
    #' @return NULL
    print = function() {
        cat(self$info(), "\n")
        for (i in head(names(self$genesets))) {
            cat(.format_str("{1} ({2})\n", i, length(self$genesets[[i]])))
        }
        invisible(self)
    },
    #' @description
    #' Return genesets as a list
    #' @return A list of genesets
    list = function() {
        return(self$genesets)
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
    #' @return A gsets object
    reduce = function(background) {
        genesets <- lapply(self$genesets, function(x) intersect(x, background))
        return(gsets$new(genesets, self$name, self$version, quiet=TRUE))
    }
))
