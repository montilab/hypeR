#' Export hyp/multihyp object to table
#'
#' @param hyp_obj A hyp or multihyp object
#' @param file_path A file path for hyp objects and directory for multihyp objects
#' @param sep The field separator string
#' @param cols Dataframe columns to include
#' @param version Add header with versioning information
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
#' hyp_to_table(hyp_obj, file_path="pathways.txt")
#'
#' @importFrom magrittr %>% extract
#' @export
hyp_to_table <- function(hyp_obj, file_path, sep="\t", cols=NULL, version=TRUE) {

    stopifnot(is(hyp_obj, "hyp") | is(hyp_obj, "multihyp"))

    # A multihyp object results in multiple tables within a director named file_path
    if (is(hyp_obj, "multihyp")) {
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

        if (version) {

            name <- hyp_obj$args$genesets$name
            version <- hyp_obj$args$genesets$version
            header <- list(.format_str("# Generated with hypeR v{1}", packageVersion("hypeR")),
                           .format_str("# Using the following genesets: {1} {2}", name, version))

            write.table(x=header, 
                        file=file_path, 
                        quote=FALSE, 
                        sep="\n",
                        col.names=FALSE, 
                        row.names=FALSE,
                        append=FALSE)

            append <- TRUE
        } else {
            append <- FALSE
        }

        suppressWarnings(write.table(x=df[,cols,drop=FALSE],
                        file=file_path,
                        quote=FALSE,
                        sep=sep,
                        col.names=TRUE,
                        row.names=FALSE,
                        append=append))
    }
}
