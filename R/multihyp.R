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
#' @importFrom R6 R6Class
#'
#' @export
multihyp <- R6Class("multihyp", list(
  data = NULL,
  initialize = function(data) {
    self$data <- data
  },
  as.list = function(...) {
    lapply(self$data, function(x) x$as.data.frame())
  }
))
