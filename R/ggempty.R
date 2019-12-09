#' An empty ggplot
#'
#' @return A ggplot object
#' 
#' @importFrom ggplot2 ggplot theme_void
#' 
#' @export
ggempty <- function() {
    ggplot() + 
    theme_void()  
}
