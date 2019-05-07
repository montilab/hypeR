#' A multihyp object
#'
#' @name multihyp
#'
#' @section Arguments:
#' \describe{
#'   \item{data}{A list of \code{hyp} objects}
#' }
#'
#' @section Methods:
#
#' \code{multihyp$as.list()} returns a list of \code{hyp} objects as dataframes.
#' 
#' @section See Also:
#' 
#' \code{hyp}
#'
#' @examples
#' data <- data.frame(replicate(5,sample(0:1,10,rep=TRUE)))
#' args <- list("arg_1"=1, "arg_2"=2, "arg_3"=3)
#' hyp_obj <- hyp$new(data, args=args)
#' data <- list("hyp_1"=hyp_obj, "hyp_2"=hyp_obj,"hyp_3"=hyp_obj)
#' multihyp_obj <- multihyp$new(data)
#'
#' @importFrom R6 R6Class
#' @export
multihyp <- R6Class("multihyp", list(
  data = NULL,
  initialize = function(data) {
    self$data <- data
  },
  print = function(...) {
    for (i in names(self$data)) {
        cat("multihyp object: \n")
        cat(i,  "\n")
        print(self$data[[i]])
    }
    invisible(self)
  },
  as.list = function(...) {
    lapply(self$data, function(x) x$as.data.frame())
  }
))
