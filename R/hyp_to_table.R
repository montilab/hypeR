#' Export hyper object to table
#'
#' @param hyp A hyper object
#' @param file.path Output file path
#' @param sep The field separator string
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
#' hyp_to_table(hyp, file.path="pathways.txt")
#'
#' @importFrom magrittr %>% extract
#' @export
hyp_to_table <- function(hyp, file.path, sep="\t", cols=NULL) {

    # Handling of multiple signatures
    if (class(hyp) == "list") {

        # Create directory if not exists
        dir.create(file.path, showWarnings=TRUE)

        # A new file for each dataframe
        for (i in names(hyp)) {
            df <- hyp[[i]]
            fname <- i

            fx = "rtf"
            if (sep == "\t") {fx <- "txt"}
            if (sep ==  ",") {fx <- "csv"}

            # File path
            fp <- paste(file.path(file.path, fname), fx, sep=".")

            # Recursive call for each dataframe
            hyp_to_table(df, fp, sep, cols)
        }
    } else  {
        if (is.null(cols)) {
            cols <- seq_len(ncol(hyp))
        }
        write.table(x = hyp[,cols,drop=FALSE],
                    file = file.path,
                    quote = FALSE,
                    sep = sep,
                    col.names = TRUE,
                    row.names = FALSE)
    }
}
