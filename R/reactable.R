#' Reactable table for hyp objects
#'
#' @param hyp A hyp object
#' @param type Use style class for outer or inner tables
#' @param show_emaps Option to show enrichment maps in tabs
#' @param show_hmaps Option to show hiearchy maps in tabs
#' @param hyp_emap_args A list of keyword arguments passed to hyp_emap
#' @param hyp_hmap_args A list of keyword arguments passed to hyp_hmap
#' 
#' @importFrom reactable reactable colDef
#' @importFrom htmltools div tagAppendChild
#' @importFrom dplyr select
#' 
#' @export
rctbl_hyp <- function(hyp,
                      type=c("inner", "outer"),
                      show_emaps=FALSE,
                      show_hmaps=FALSE,
                      hyp_emap_args=list(top=25, val="fdr"),
                      hyp_hmap_args=list(top=25, val="fdr")) {

    type <- match.arg(type)
    class.obj    <- .format_str("rctbl-{1}-obj", type)
    class.tbl    <- .format_str("rctbl-{1}-tbl", type)
    class.header <- .format_str("rctbl-{1}-header", type)

    df <- dplyr::select(hyp$data, c("label", "pval", "fdr", "geneset", "overlap", "hits"))
    tbl <- reactable(df,
                     rownames=FALSE,
                     resizable=TRUE,
                     showPageSizeOptions=FALSE,
                     compact=TRUE, 
                     defaultColDef=colDef(headerClass=class.header),
                     columns=list(label   = colDef(name="Label", minWidth=300),
                                  pval    = colDef(name="P-Value"),
                                  fdr     = colDef(name="FDR"),
                                  geneset = colDef(name="Geneset"),
                                  overlap = colDef(name="Overlap"),
                                  hits    = colDef(name="Hits")),
                     class=class.tbl)

    dat <- htmltools::div(class=class.obj, tbl)
    
    if (show_emaps) {
        hyp_emap_args[['hyp_obj']] <- hyp
        viz <- do.call(hyp_emap, hyp_emap_args)
        dat <- htmltools::tagAppendChild(dat, viz)
    }

    if (show_hmaps) {
        hyp_hmap_args[['hyp_obj']] <- hyp
        viz <- do.call(hyp_hmap, hyp_hmap_args)
        dat <- htmltools::tagAppendChild(dat, viz)
    }

    return(dat)
}

#' Reactable table for multihyp objects
#'
#' @param mhyp A multihyp object
#' @param show_emaps Option to show enrichment maps in tabs
#' @param show_hmaps Option to show hiearchy maps in tabs
#' @param hyp_emap_args A list of keyword arguments passed to hyp_emap
#' @param hyp_hmap_args A list of keyword arguments passed to hyp_hmap
#' 
#' @importFrom reactable reactable colDef
#' @importFrom htmltools div
#' 
#' @export
rctbl_mhyp <- function(mhyp,
                       show_emaps=FALSE,
                       show_hmaps=FALSE,
                       hyp_emap_args=list(top=25, val="fdr"),
                       hyp_hmap_args=list(top=25, val="fdr")) {

    mhyp.df <- data.frame(signature=names(mhyp$data), 
                          size=sapply(mhyp$data, function(hyp) hyp$info[["Signature Size"]]),
                          gsets=sapply(mhyp$data, function(hyp) hyp$info[["Genesets"]]),
                          bg=sapply(mhyp$data, function(hyp) hyp$info[["Background"]]))
    
    tbl <- reactable(
        mhyp.df,
        showPageSizeOptions = FALSE,
        onClick = "expand",
        resizable = TRUE,
        rownames = FALSE,
        defaultColDef = colDef(headerClass="rctbl-outer-header"),
        columns = list(signature = colDef(name="Signature", minWidth=300),
                       size = colDef(name="Size"),
                       gsets = colDef(name="Genesets"),
                       bg = colDef(name="Background")),
        
        details = function(index) {
                hyp <- mhyp$data[[index]]
                rctbl_hyp(hyp, type="inner", show_emaps, show_hmaps, hyp_emap_args, hyp_hmap_args)
            },
        wrap = FALSE,
        class = "rctbl-outer-tbl",
        rowStyle = list(cursor="pointer")
        )
    
    htmltools::div(class="rctbl-outer-obj", tbl) 
}

#' Reactable builder for hyp or mhyp objects
#' 
#' @param obj A hyp or multihyp object
#' @param ... Arguments passed to table generators 
#' 
#' @export
rctbl_build <- function(obj, ...) {
    if (is(obj, "hyp")) {
        return(rctbl_hyp(obj, type="outer", ...))
    }
    if (is(obj, "multihyp")) {
        return(rctbl_mhyp(obj, ...))
    }
}
