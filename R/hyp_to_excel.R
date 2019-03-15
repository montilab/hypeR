#' Export hyp object to excel
#'
#' @param hyp A hyp or multihyp object
#' @param file_path Output file path
#' @param cols Dataframe columns to include
#' @return None
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
#' hyp_to_excel(hyp, file_path="pathways.xlsx")
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @export
hyp_to_excel <- function(hyp, file_path, cols=NULL) {
    
    stopifnot(class(hyp) == "hyp" | class(hyp) == "multihyp")

    # Handle hyp objects
    if (class(hyp) == "hyp") {
        multihyp <- new("multihyp", data=list(" " = hyp))
    } else {
        multihyp <- hyp
    }
    
    # Generate excel file
    wb <- openxlsx::createWorkbook()

    # A new sheet for each dataframe
    for (i in names(multihyp@data)) {
        sheet <- openxlsx::addWorksheet(wb, sheetName=i)

        # Extract hyp dataframe
        hyp <- multihyp@data[[i]]
        df <- hyp@data

        if (is.null(cols)) {
            cols <- seq_len(ncol(df))
        }
        x <- df[,cols,drop=FALSE]
        openxlsx::writeData(wb, sheet=i, x=x, colNames=TRUE, rowNames=FALSE)
    }
    openxlsx::saveWorkbook(wb, file=file_path, overwrite=TRUE)
}
