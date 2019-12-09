#' Enrichment plot implemented in ggplot
#'
#' @param n The length of a ranked list
#' @param positions A vector of positions in the ranked list
#' @param x_axis The x-axis of a running enrichment score
#' @param y_axis The y-axis of a running enrichment score
#' @param title Plot title
#' @return A ggplot object
#' 
#' @importFrom ggplot2 qplot aes geom_rug geom_hline geom_vline annotate theme element_text element_blank element_line element_rect
#' 
#' @export
ggeplot <- function(n, positions, x_axis, y_axis, title="") {
    score <- which.max(abs(y_axis))
    qplot(x_axis, 
          y_axis,
          main=title,
          ylab="Running Enrichment Score", 
          xlab="Position in Ranked List of Genes",
          geom="line")+
    geom_rug(data=data.frame(positions), aes(x=positions), inherit.aes=FALSE)+
    geom_hline(yintercept=0) +
    geom_vline(xintercept=n/2, linetype="dotted") +
    annotate("point", x=x_axis[score], y=y_axis[score], color="red") +
    annotate("text", x=x_axis[score]+n/20, y=y_axis[score], label=round(y_axis[score],2)) +
    annotate("point", x=x_axis[score], y=y_axis[score], color="red") +
    theme(plot.title=element_text(hjust=0.5),
          panel.background=element_blank(), 
          axis.line=element_line(color="black"),
          panel.border=element_rect(color="black", fill=NA, size=1))    
}
