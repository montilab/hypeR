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

rmd_tab <- "
### {1} 
```{r {2}, fig.width=8.25, fig.align='center'}
hyp_obj <- tabsets[['{3}']][['{1}']] 
{4}
{5}
{6}
{7}
{8}
```
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

rmd_versioning <- "
info <- hyp_obj$info
mat <- matrix(c(names(hyp_obj$info), as.character(hyp_obj$info)), ncol=2)
df <- as.data.frame(mat)
colnames(df) <- NULL
df
"

rmd_session <- "
# Session Info
```{r}
sessionInfo()
```
"

#' Export hyp object to rmarkdown
#'
#' @param hyp_obj A hyp object, multihyp object, or list of multihyp objects
#' @param file_path A file path
#' @param title Title of markdown report
#' @param subtitle Subtitle of markdown report
#' @param author Authors of markdown report
#' @param header Header name of tabset section
#' @param versioning Add versioning information
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
#' @param session_info Use true to include session info
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
                       versioning=TRUE,
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
                       custom_post_content=NULL,
                       session_info=FALSE) {
    
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
        
            dots_area <- ifelse(show_dots,  .format_str(tab_dots, .string_args(hyp_dots_args)), "")
            data_area <- ifelse(show_tables, tab_table, "")
            emap_area <- ifelse(show_emaps, .format_str(tab_emap, .string_args(hyp_emap_args)), "")
            hmap_area <- ifelse(show_hmaps, .format_str(tab_hmap, .string_args(hyp_hmap_args)), "")
            vers_area <- ifelse(versioning, rmd_versioning, "")

            rmd_tab %>%
            .format_str(tab, tab_id, tabset, dots_area, data_area, emap_area, hmap_area, vers_area) %>%
            write(file=file_path, append=TRUE)
        }
    }

    # Session info
    if (session_info) write(rmd_session, file=file_path, append=TRUE)
    
    # Trailing content
    if (!is.null(custom_post_content)) write(custom_post_content, file=file_path, append=TRUE)

    rmarkdown::render(input=file_path, 
                      output_format="html_document",
                      output_file=paste(file_path, "html", sep="."))
}
