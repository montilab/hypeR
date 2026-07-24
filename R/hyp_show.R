#' Convert a hyp object to a reactable table
#'
#' @param hyp_obj A hyp object
#' @param simple Use true to only include essential columns
#' @return A reactable table
#'
#' @examples
#' genesets <- msigdb_gsets("Homo sapiens", "C2", "CP:KEGG_LEGACY")
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
#' 
#' @export
hyp_show <- function(hyp_obj, simple=FALSE) {
    stopifnot(is(hyp_obj, "hyp"))

    # Extract hyp dataframe
    df <- hyp_obj$data

    # Pretty column names
    col_names <- colnames(df)
    fixed_cols <- c("label", "es", "pval", "fdr")
    lower_cols <- tolower(col_names)
    col_names[lower_cols == "label"] <- "Label"
    col_names[lower_cols == "es"] <- "ES"
    col_names[lower_cols == "pval"] <- "P-Value"
    col_names[lower_cols == "fdr"] <- "FDR"
    remaining <- !(lower_cols %in% fixed_cols)
    col_names[remaining] <- str_to_title(col_names[remaining])
    colnames(df) <- col_names

    cols <- if(simple) c(1,2,3) else seq_len(ncol(df))
    table_data <- df[, cols, drop=FALSE]
    table <- reactable(data=table_data,
                       searchable=TRUE,
                       compact=TRUE, 
                       fullWidth=TRUE,
                       defaultPageSize=15,
                       pageSizeOptions=c(15, 25, 50, 100),
                       striped=TRUE,
                       showPageSizeOptions=TRUE)
    table$data <- table_data
    table
}
