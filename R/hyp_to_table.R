#' Export hyp or multihyp object to table
#'
#' @param hyp_obj A hyp or multihyp object
#' @param file_path Output file path
#' @param sep The field separator string
#' @param cols Dataframe columns to include
#' @return None
#'
#' @examples
#' # Grab a list of curated gene sets
#' gsets <- readRDS(system.file("extdata/gsets.rds", package="hypeR"))
#' REACTOME <- gsets$REACTOME
#'
#' # Genes involed in tricarboxylic acid cycle
#' symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
#'              "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
#'              "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")
#'
#' # Perform hyper enrichment
#' hyp_obj <- hypeR(symbols, REACTOME, bg=2522, fdr=0.05)
#'
#' # Export
#' hyp_to_table(hyp_obj, file_path="pathways.txt")
#'
#' @importFrom magrittr %>% extract
#' @export
hyp_to_table <- function(hyp_obj, file_path, sep="\t", cols=NULL) {

    stopifnot("hyp" %in% class(hyp_obj) | "multihyp" %in% class(hyp_obj))


    # A multihyp object results in multiple tables within a director named file_path
    if ("multihyp" %in% class(hyp_obj)) {
        multihyp_obj <- hyp_obj

        # Create directory if not exists
        dir.create(file_path, showWarnings=TRUE)

        # A new file for each dataframe
        for (i in names(multihyp_obj$data)) {
            
            fname <- i
            fx = "rtf"
            if (sep == "\t") {fx <- "txt"}
            if (sep ==  ",") {fx <- "csv"}
            fp <- paste(file.path(file_path, fname), fx, sep=".")

            # Extract hyp object
            hyp_obj <- multihyp_obj$data[[i]]

            # Recursive call for each hyp object
            hyp_to_table(hyp_obj, fp, sep, cols)
        }

    # A hyp object results in a single table named file_path
    } else  {
        # Extract hyp dataframe
        df <- hyp_obj$data
        if (is.null(cols)) {
            cols <- seq_len(ncol(df))
        }
        write.table(x = df[,cols,drop=FALSE],
                    file = file_path,
                    quote = FALSE,
                    sep = sep,
                    col.names = TRUE,
                    row.names = FALSE)
    }
}
