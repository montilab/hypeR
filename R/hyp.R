#' @title A hyp object
#' 
#' @examples
#' data <- data.frame(replicate(5,sample(0:1,10,rep=TRUE)))
#' args <- list("arg_1"=1, "arg_2"=2, "arg_3"=3)
#' hyp_obj <- hyp$new(data, args=args)
#' 
#' @section See Also:
#' 
#' \code{multihyp}
#'
#' @importFrom R6 R6Class
#' 
#' @export
hyp <- R6Class("hyp", list(
    #' @field data A dataframe returned by hypeR()
    #' @field plots A list of plots returned by hypeR()
    #' @field args A list of arguments passed to hypeR()
    #' @field info Exported information for reproducibility
    data  = NULL,
    plots = NULL,
    args  = NULL,
    info  = NULL,

    #' @description
    #' Create a hyp object
    #' @param data A dataframe returned by hypeR()
    #' @param plots A list of plots returned by hypeR()
    #' @param args A list of arguments passed to hypeR()
    #' @param info Exported information for reproducibility
    #' @return A new hyp object  
    initialize = function(data, plots=NULL, args=NULL, info=NULL) {
        self$data  <- data
        self$plots <- plots
        self$args  <- args
        self$info  <- info
    },
    #' @description
    #' Print hyp obect
    #' @return NULL
    print = function() {
        cat("(hyp) \n\n")
        cat("  data: \n")
        base::print(head(self$data), row.names=FALSE)
        cat(.format_str("\n  plots: {1} Figures\n\n", length(self$plots)))
        cat(.format_str("  args: {1}", paste(names(self$args), collapse="\n        ")))
        invisible(self)
    },
    #' @description
    #' Extract dataframe from hyp obect
    #' @return NULL
    as.data.frame = function() {
        self$data
    }
))
