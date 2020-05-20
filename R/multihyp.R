#' @title A multihyp object
#'
#' @examples
#' data <- data.frame(replicate(5,sample(0:1,10,rep=TRUE)))
#' args <- list("arg_1"=1, "arg_2"=2, "arg_3"=3)
#' hyp_obj <- hyp$new(data, args=args)
#' data <- list("hyp_1"=hyp_obj, "hyp_2"=hyp_obj,"hyp_3"=hyp_obj)
#' multihyp_obj <- multihyp$new(data)
#'
#' @section See Also:
#' 
#' \code{hyp}
#' 
#' @importFrom R6 R6Class
#' 
#' @export
multihyp <- R6Class("multihyp", list(
    #' @field data A list of hyp objects
    data = NULL,

    #' @description
    #' Create a multihyp object
    #' @param data A list of hyp objects
    #' @return A new multihyp object  
    initialize = function(data) {
        self$data <- data
    },
    #' @description
    #' Print multihyp obect
    #' @return NULL
    print = function() {
        cat("(multihyp) \n")
        for (i in names(self$data)) {
            dims <- dim(self$data[[i]]$data)
            cat(.format_str("  (hyp) {1}\n", i))
            cat(.format_str("        {1} x {2}\n", dims[1], dims[2]))
        }
        invisible(self)
    },
    #' @description
    #' Print multihyp obect
    #' @return A list of hyp objects as dataframes
    as.list = function() {
        lapply(self$data, function(x) x$as.data.frame())
    }
))
