rmd_config <- "---
title: '{1}'
subtitle: '{2}'
author: '{3}'
output:
  html_document:
    theme: united
    toc: true
    toc_float: true
    code_folding: hide
    toc_depth: 1
    df_print: paged
---

**Generated with hypeR**: v`r packageVersion('hypeR')`  
**Date Generated**: `r Sys.Date()`

***
"

rmd_knitr <- "
```{r setup, echo=FALSE, message=FALSE}
knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE)
library(kableExtra)
```
"

rmd_tabset <- "
# {1}
##  {.tabset .tabset-fade}
"

rmd_versioning <- "
**Using the following genesets**: `r tabsets[['{3}']][['{1}']]$args$genesets$name` `r tabsets[['{3}']][['{1}']]$args$genesets$version`
"

tab_dots <- "
hyp_obj %>%
hyp_dots({1})
"

tab_emap <- "
hyp_obj %>%
hyp_emap({1})
"

tab_hmap <- "
hyp_obj %>%
hyp_hmap({1})
"

tab_table <- "
df <- hyp_obj$as.data.frame()
df$label.abrv <- substr(df$label, 1, 30) 
col_ix <- match(c('pval', 'fdr', 'label.abrv'), colnames(df))
df <- df[, c(col_ix, (1:ncol(df))[-col_ix])]
df$label <- NULL
rownames(df) <- NULL
df
"

#' Export hyp object to rmarkdown
#'
#' @param hyp_obj A hyp object, multihyp object, or list of multihyp objects
#' @param file_path A file path
#' @param title Title of markdown report
#' @param subtitle Subtitle of markdown report
#' @param author Authors of markdown report
#' @param header Header name of tabset section
#' @param version Add versioning information
#' @param show_dots Option to show dots plots in tabs
#' @param show_emaps Option to show enrichment maps in tabs
#' @param show_hmaps Option to show hiearchy maps in tabs
#' @param show_tables Option to show table in tabs
#' @param hyp_dots_args A list of keyword arguments passed to hyp_dots
#' @param hyp_emap_args A list of keyword arguments passed to hyp_emap
#' @param hyp_hmap_args A list of keyword arguments passed to hyp_hmap
#' @param custom_rmd_config Replace configuration section of markdown report
#' @param custom_pre_content Insert custom content before tabset section
#' @param custom_post_content Insert custom content after tabset section
#' @return NULL
#'
#' @import kableExtra
#' @importFrom rmarkdown render
#' @importFrom magrittr %>%
#' 
#' @export
hyp_to_rmd <- function(hyp_obj,
                       file_path,
                       title="Workflow Report",
                       subtitle="",
                       author="",
                       header="Results",
                       version=TRUE,
                       show_dots=TRUE,
                       show_emaps=TRUE,
                       show_hmaps=FALSE,
                       show_tables=TRUE,
                       hyp_dots_args=list(top=15, 
                                          val="fdr"),
                       hyp_emap_args=list(top=25, 
                                          val="fdr", 
                                          similarity_metric="jaccard_similarity", 
                                          similarity_cutoff=0.2),
                       hyp_hmap_args=list(top=25,
                                          val="fdr"),    
                       custom_rmd_config=NULL,
                       custom_pre_content=NULL,
                       custom_post_content=NULL) {
    
    # Markdown configuration
    if (!is.null(custom_rmd_config)) {
        rmd_config <- custom_rmd_config
    } else {
        rmd_config <- .format_str(rmd_config, title, subtitle, author)
    }

    write(rmd_config, file=file_path, append=FALSE)
    write(rmd_knitr, file=file_path, append=TRUE)

    # Content before hyper enrichment tabs
    if (!is.null(custom_pre_content)) {
        write(custom_pre_content, file = file_path, append = TRUE)
    }

    # Iterate over the following object structure
    #
    # Generate a tab...
    # for each multihyp
    #    for each hyp
    #
    # Example:
    # list(tabset1 = list(tab1 = hyp,
    #                     tab2 = hyp),
    #      tabset2 = list(tab1 = hyp,
    #                     tab2 = hyp))

    # A single set of tabs
    # ----------------------
    if (is(hyp_obj, "hyp")) {
        tabsets <- list(x = list(" " = hyp_obj))
        names(tabsets) <- c(header)
    }
    if (is(hyp_obj, "multihyp")) {
        tabsets <- list(x = hyp_obj$data)
        names(tabsets) <- c(header)
    }
    if (is(hyp_obj, "list")) {
        tabsets <- lapply(hyp_obj, function(x) {
            stopifnot(is(x, "hyp")| is(x, "multihyp"))
            if (is(x, "hyp")) {
                return(list(" " = x))
            }
            if (is(x, "multihyp")) {
                return(x$data)
            }
        })
    }
    # ----------------------

    # Limit is 10,000 tabs
    ids <- pvector$new(seq(10000))

    for (tabset in names(tabsets)) {

        # Tabset header and init code
        rmd_tabset %>%
        .format_str(tabset) %>%
        write(file=file_path, append=TRUE)

        # Tab content
        tabs <- tabsets[[tabset]]
        for (tab in names(tabs)) {

            tab_id <- ids$pop()
            
            rmd_tab <- "### {1}"
            if (version) rmd_tab <- paste(rmd_tab, rmd_versioning)
            rmd_tab <- paste(rmd_tab, "```{r {2}, fig.width=8.25, fig.align='center'}", sep="")
            rmd_tab <- paste(rmd_tab, "hyp_obj <- tabsets[['{3}']][['{1}']]", sep="\n")
            rmd_tab <- .format_str(rmd_tab, tab, tab_id, tabset)

            if (show_dots) {
                rmd_tab <- paste(rmd_tab, .format_str(tab_dots, .string_args(hyp_dots_args)), sep="\n")
            }
            if (show_tables) {
                rmd_tab <- paste(rmd_tab, tab_table, sep="\n")
            }
            if (show_emaps) {
                rmd_tab <- paste(rmd_tab, .format_str(tab_emap, .string_args(hyp_emap_args)), sep="\n")
            }
            if (show_hmaps) {
                rmd_tab <- paste(rmd_tab, .format_str(tab_hmap, .string_args(hyp_hmap_args)), sep="\n")
            }

            rmd_tab <- paste(rmd_tab, "```\n", sep="\n")

            write(rmd_tab, file=file_path, append=TRUE)
        }
    }

    # Content after hyper enrichment tabs
    if (!is.null(custom_post_content)) {
        write(custom_post_content, file=file_path, append=TRUE)
    }

    rmarkdown::render(input=file_path, 
                      output_format="html_document",
                      output_file=paste(file_path, "html", sep="."))
}
