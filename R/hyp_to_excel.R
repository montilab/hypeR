#' Export hyp/multihyp object to excel
#'
#' @param hyp_obj A hyp or multihyp object
#' @param file_path A file path
#' @param cols Dataframe columns to include
#' @param version Add sheet with versioning information
#' @return NULL
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
#' hyp_to_excel(hyp_obj, file_path="pathways.xlsx")
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @export
hyp_to_excel <- function(hyp_obj, file_path, cols=NULL, version=TRUE) {
    
    stopifnot(is(hyp_obj, "hyp") | is(hyp_obj, "multihyp"))

    # Handle hyp objects
    if (is(hyp_obj, "hyp")) {
        multihyp_obj <- multihyp$new(data=list(" "=hyp_obj))
    } else {
        multihyp_obj <- hyp_obj
    }
    
    # Generate excel file
    wb <- openxlsx::createWorkbook()

    # A new sheet for each dataframe
    for (i in names(multihyp_obj$data)) {
        sheet <- openxlsx::addWorksheet(wb, sheetName=i)

        # Extract hyp dataframe
        hyp_obj <- multihyp_obj$data[[i]]
        df <- hyp_obj$data

        if (is.null(cols)) {
            cols <- seq_len(ncol(df))
        }
        x <- df[,cols,drop=FALSE]

        openxlsx::writeData(wb, sheet=i, x=x, colNames=TRUE, rowNames=FALSE)
    }

    if (version) {
        name <- hyp_obj$args$genesets$name
        version <- hyp_obj$args$genesets$version
        header <- matrix(c(.format_str("Generated with hypeR v{1}", packageVersion("hypeR")),
                           .format_str("Using the following genesets: {1} {2}", name, version)))    
                  
        sheet <- openxlsx::addWorksheet(wb, sheetName="version")     
        openxlsx::writeData(wb, sheet="version", x=header, colNames=FALSE, rowNames=FALSE)        
    }

    suppressMessages(openxlsx::saveWorkbook(wb, file=file_path, overwrite=TRUE))
}
