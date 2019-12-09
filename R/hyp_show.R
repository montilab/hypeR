#' Convert a hyp object to a reactable table
#'
#' @param hyp_obj A hyp object
#' @param simple Use true to only include essential columns
#' @return A reactable table
#'
#' @examples
#' genesets <- msigdb_gsets("Homo sapiens", "C2", "CP:KEGG")
#'
#' signature <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'                "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'                "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' hyp_obj <- hypeR(signature, genesets, background=2522)
#'
#' hyp_show(hyp_obj)
#'
#' @importFrom reactable reactable
#' @importFrom stringr str_to_title
#' @export
hyp_show <- function(hyp_obj, simple=FALSE) {
    stopifnot(is(hyp_obj, "hyp"))

    # Extract hyp dataframe
    df <- hyp_obj$data

    # Pretty column names
    colnames(df) <- c("Label", "P-Value", "FDR", str_to_title(colnames(df)[4:ncol(df)]))

    cols <- if(simple) c(1,2,3) else seq_len(ncol(df))

    reactable(data=df[,cols,drop=FALSE],
              searchable=TRUE,
              compact=TRUE, 
              fullWidth=TRUE,
              defaultPageSize=15,
              pageSizeOptions=c(15, 25, 50, 100),
              striped=TRUE,
              showPageSizeOptions=TRUE)
}
