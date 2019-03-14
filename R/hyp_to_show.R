#' Convert hyper dataframe to an interactive datatable
#'
#' @param df A hyper dataframe
#' @param simple Use true to only include essential dataframe columns
#' @param stylish Use true to add a bootstrap styling theme to datatable
#' @return A datatable object
#'
#' @examples
#' # Grab a list of curated gene sets
#' REACTOME <- ex_get("C2.CP.REACTOME")
#'
#' # Genes involed in tricarboxylic acid cycle
#' symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'              "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'              "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' # Perform hyper enrichment
#' hyp <- hypeR(symbols, REACTOME, bg=2522, fdr=0.05)
#'
#' # Export
#' hyp_show(hyp)
#'
#' @importFrom DT datatable
#' @export
hyp_show <- function(df, simple=TRUE, stylish=FALSE) {
    if (simple) {
        cols <- c(1,2,7,8)
    } else {
        cols <- seq_len(ncol(df))
    }

    # Gene symbols converted to hyperlinks
    url <- "https://www.genecards.org/cgi-bin/carddisp.pl?gene="
    df$hits <- lapply(df$hits, function(x) {
                   vapply(symbols, function(x) {
                       paste('<a href="',
                             url,
                             x,
                             '">',
                             x,
                             '</a>',
                             sep="")
                   }, character(1))
               })

    if (stylish) {
        datatable(data = df[,cols,drop=FALSE],
                  style = 'bootstrap',
                  class = 'table-bordered table-condensed',
                  escape = TRUE,
                  rownames = FALSE)
    } else {
        datatable(data = df[,cols,drop=FALSE],
                  escape = TRUE,
                  rownames = FALSE)
    }
}
