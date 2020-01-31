#' A push/pop capable vector
#'
#' @importFrom R6 R6Class
#' 
#' @keywords internal
pvector <- R6Class("pvector", list(
    #' @field values A vector of values
    values = NULL,

    #' @description
    #' Create a pvector
    #' @param values A vector of values
    #' @return A new pvector  
    initialize = function(values=c()) {
        self$values <- values
    },
    #' @description
    #' Print pvector
    #' @return NULL  
    print = function() {
        base::print(self$values)
        invisible(self)
    },
    #' @description
    #' Get length of pvector
    #' @return An integer  
    length = function() {
        base::length(self$values)
    },
    #' @description
    #' Pop vector
    #' @return Popped value 
    pop = function() {
        if (length(self$values) > 0) {
            popped.value <- self$values[1]
            self$values <- self$values[-1]
            return(popped.value)   
        }
    },
    #' @description
    #' Push values
    #' @param pushed.values A vector of values
    #' @return NULL
    push = function(pushed.values) {
        self$values <- c(self$values, pushed.values)
    }
))
