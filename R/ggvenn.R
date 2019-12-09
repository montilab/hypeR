#' Venn diagram implemented in ggplot
#'
#' @param a A vector for group a
#' @param b A vector for group b
#' @param ga A string label for group a
#' @param gb A string label for group b
#' @param title Plot title
#' @return A ggplot object
#'
#' @importFrom igraph groups
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes coord_fixed theme_void theme labs element_text
#' @importFrom ggforce geom_circle
#' 
#' @export
ggvenn <- function(a, b, ga, gb, title="") {
   
    # Calculate area
    x.a <- length(a)
    x.b <- length(b)
    x.u <- length(intersect(a, b))
    
    # Normalize area between 0-1
    nx.a <- x.a/(x.a+x.b)
    nx.b <- x.b/(x.a+x.b)
    
    # Circle radius and diameter
    r.a <- sqrt(nx.a/pi); d.a <- r.a*2
    r.b <- sqrt(nx.b/pi); d.b <- r.b*2
    
    # Overlay circles by percent overlap of smaller group
    ol <- ifelse(x.a < x.b, d.a*x.u/x.a, d.b*x.u/x.b)
    
    data.frame(x=c(-r.b-r.a+ol, 0),
               y=c(0, 0),
               groups=c(paste(ga, " (", x.a, ")", sep=""),
                        paste(gb, " (", x.b, ")", sep=""))) %>%
        
    ggplot(aes(x0=x, y0=y, r=c(r.a, r.b), fill=groups)) +
    geom_circle(alpha=0.3, size=0.5) +
    coord_fixed() +
    theme_void() +
    theme(plot.title=element_text(hjust=0.5),
          plot.subtitle=element_text(hjust=0.5)) +
    labs(title=title, subtitle=paste("Overlap", " (", x.u, ")", sep=""))
}
