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
ggeplot <- function(n, positions, x_axis, y_axis, title = "") {
  score <- which.max(abs(y_axis))
  DF <- data.frame(x_axis = x_axis, y_axis = y_axis)
  ggplot2::ggplot(DF, aes(x = x_axis, y = y_axis)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = title,
      y = "Running Enrichment Score",
      x = "Position in Ranked List of Genes"
    ) +
    ggplot2::geom_rug(data = data.frame(positions), aes(x = positions), inherit.aes = FALSE) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = n / 2, linetype = "dotted") +
    ggplot2::annotate("point", x = x_axis[score], y = y_axis[score], color = "red") +
    ggplot2::annotate("text", x = x_axis[score] + n / 20, y = y_axis[score], label = round(y_axis[score], 2)) +
    ggplot2::annotate("point", x = x_axis[score], y = y_axis[score], color = "red") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
    )
}
