#' Export hyper object to excel
#'
#' @param hyp A hyper object
#' @param file.path Output file path
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
#' hyp_to_excel(hyp, file.path="pathways.xlsx")
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @export
hyp_to_excel <- function(hyp, file.path, cols=NULL) {
    
    # Handle single dataframe hyper objects
    if (class(hyp) == "data.frame") {
        hyp <- list(" " = hyp)
    }
    
    # Generate excel file
    wb <- openxlsx::createWorkbook()

    # A new sheet for each dataframe
    for (i in names(hyp)) {
        sheet <- openxlsx::addWorksheet(wb, sheetName=i)
        df <- hyp[[i]]
        if (is.null(cols)) {
            cols <- seq_len(ncol(df))
        }
        x <- df[,cols,drop=FALSE]
        openxlsx::writeData(wb, sheet=i, x=x, colNames=TRUE, rowNames=FALSE)
    }
    openxlsx::saveWorkbook(wb, file=file.path, overwrite=TRUE)
}
