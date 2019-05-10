#' Convert hyp object to an interactive datatable
#'
#' @param hyp_obj A hyp object
#' @param simple Use true to only include essential dataframe columns
#' @param stylish Use true to add a bootstrap styling theme to datatable
#' @return A datatable object
#'
#' @examples
#' gsets <- readRDS(file.path(system.file("extdata", package="hypeR"), "hypdat.rds"))$gsets
#'
#' signature <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'                "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'                "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' # Perform hyper enrichment
#' hyp_obj <- hypeR(signature, gsets, bg=2522, fdr_cutoff=0.05)
#'
#' # Export
#' hyp_show(hyp_obj)
#'
#' @importFrom DT datatable
#' @export
hyp_show <- function(hyp_obj, simple=FALSE, stylish=FALSE) {

    stopifnot(is(hyp_obj, "hyp"))

    # Extract hyp dataframe
    df <- hyp_obj$data

    cols <- if(simple) c(1,2,3) else seq_len(ncol(df))

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
