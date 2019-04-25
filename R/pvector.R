#' A push/pop capable vector
#'
#' @name pvector
#'
#' @section Arguments:
#' \describe{
#'   \item{values}{A vector of values}
#' }
#'
#' @section Methods:
#' \code{pvector$pop()} Pop vector.
#'
#' \code{pvector$push()} Push vector.
#'
#' @examples
#' pv <- pvector$new(c(1,2,3))
#' popped <- pv$pop()
#' pv$push(4)
#' pv$push(c(5,6))
#' print(pv$values) 
#'
#' @importFrom R6 R6Class
#'
#' @export
pvector <- R6Class("pvector", list(
    values = NULL,
    initialize = function(values=c()) {
        self$values <- values
    },
    pop = function() {
        if (length(self$values) > 0) {
            popped.value <- self$values[1]
            self$values <- self$values[-1]
            return(popped.value)   
        }
    },
    push = function(pushed.values) {
        self$values <- c(self$values, pushed.values)
    }
))
