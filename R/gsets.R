#' A genesets object
#'
#' @name gsets
#'
#' @section Arguments:
#' \describe{
#'   \item{genesets}{A named list of genesets}
#'   \item{name}{A character vector describing source of genesets}
#'   \item{version}{A character vector describing versioning}
#' }
#'
#' @section Methods:
#' 
#' \code{print(gsets)} prints geneset information.
#'
#' \code{gsets$reduce(background)} reduces genesets to a background distribution of symbols.
#' 
#' @section See Also:
#' 
#' \code{rgsets}
#' 
#' @examples
#' genesets <- list("GSET1" = c("GENE1", "GENE2", "GENE3"),
#'                  "GSET2" = c("GENE4", "GENE5", "GENE6"),
#'                  "GSET3" = c("GENE7", "GENE8", "GENE9"))
#'
#' gsets_obj <- gsets$new(genesets, name="example", version="v1.0")
#' print(gsets_obj)
#'
#' @importFrom R6 R6Class
#' 
#' @export
gsets <- R6Class("gsets", list(
    genesets = NULL,
    name = NULL,
    version = NULL,
    initialize = function(genesets, name="", version="", quiet=FALSE) {
        if (!is(genesets, "list")) stop("Genesets must be a named list of symbols")
        if (is.null(names(genesets))) stop("Genesets must be a named list of symbols")
        if (name == "" & !quiet) warning("Describing genesets with a name will aid reproducibility")
        if (version == "" & !quiet) warning("Including a version number will aid reproducibility")
        self$genesets <- genesets
        self$name <- name
        self$version <- version
    },
    print = function(...) {
        if (self$name != "") cat(.format_str("{1} ", self$name))
        if (self$version != "") cat(.format_str("{1}", self$version))
        if (self$name != "" | self$version != "") cat("\n")
        for (i in head(names(self$genesets))) {
            cat(.format_str("{1} ({2})\n", i, length(self$genesets[[i]])))
        }
        invisible(self)
    },
    reduce = function(background) {
        genesets <- lapply(self$genesets, function(x) intersect(x, background))
        return(gsets$new(genesets, self$name, self$version, quiet=TRUE))
    }
))
