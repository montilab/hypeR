#' Format a string using placeholders
#'
#' @param string A an unformatted string with placeholders
#' @param ... Variables to format placeholders with
#' @return A formatted string
#' 
#' @examples
#' \dontrun{
#' format_str("Format with {1} and {2}", "x", "y")
#' }
#'
#' @keywords internal
format_str <- function(string, ...) {
    args <- list(...)
    for (i in 1:length(args)) {
        pattern <- paste("\\{", i, "}", sep="")
        replacement <- args[[i]]
        string <- gsub(pattern, replacement, string)
    }
    return(string)
}

#' Convert an arguments list to string format
#'
#' @param args A list of keyword arguments
#' @return A string of keyword arguments
#'
#' @examples
#' \dontrun{
#' string_args(list(x=15, y="fdr", z=TRUE))
#' }
#'
#' @keywords internal
string_args <- function(args) {
    paste(paste(names(args), 
                sapply(unname(args), function(x) {
                    if (is.character(x)) {shQuote(x)}
                    else if (is.null(x)) {"NULL"}
                    else {x}
                }), 
                sep='='), 
                collapse=",")
}

rmd_config <- "---
title: '{1}'
subtitle: '{2}'
author: '{3}'
date: 'Last Modified: `r Sys.Date()`'
output:
  html_document:
    theme: united
    toc: true
    toc_float: true
    toc_depth: 1
    code_folding: hide
    df_print: paged
---
"

rmd_knitr <- "
```{r setup, echo=FALSE, message=FALSE}
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
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
hyp.obj <- tabsets[['{3}']][['{1}']] 
{4}
{5}
{6}
{7}
```
"

tab_plot <- "
hyp.obj %>%
hyp_plot({1})
"

tab_emap <- "
hyp.obj %>%
hyp_emap({1})
"

tab_hmap <- "
hyp.obj %>%
hyp_hmap({1})
"

tab_table <- "
df <- hyp.obj$as.data.frame()
df$abrv.name <- substr(rownames(df), 1, 30) 
col_ix <- match(c('pval', 'fdr', 'abrv.name'), colnames(df))
df <- df[, c(col_ix, (1:ncol(df))[-col_ix])]
rownames(df) <- NULL
df
"

#' Export hyp object to rmarkdown
#'
#' @param hyp.obj A hyp object, multihyp object, or list of multihyp objects
#' @param file_path Output file path
#' @param title Title of markdown report
#' @param subtitle Subtitle of markdown report
#' @param author Authors of markdown report
#' @param header Header name of tabset section
#' @param show_plots Option to show plots in tabs
#' @param show_emaps Option to show enrichment maps in tabs
#' @param show_hmaps Option to show hiearchy maps in tabs
#' @param show_tables Option to show table in tabs
#' @param hyp_plot_args A list of keyword arguments passed to hyp_plot
#' @param hyp_emap_args A list of keyword arguments passed to hyp_emap
#' @param hyp_hmap_args A list of keyword arguments passed to hyp_hmap
#' @param custom_rmd_config Replace configuration section of markdown report
#' @param custom_pre_content Insert custom content before tabset section
#' @param custom_post_content Insert custom content after tabset section
#' @return None
#'
#' @importFrom rmarkdown render
#' @importFrom magrittr %>%
#' @export
hyp_to_rmd <- function(hyp.obj,
                       file_path,
                       title="hypeR Enrichment Report",
                       subtitle="",
                       author="",
                       header="Enrichment",
                       show_plots=TRUE,
                       show_emaps=TRUE,
                       show_hmaps=FALSE,
                       show_tables=TRUE,
                       hyp_plot_args=list(top=15, 
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

    # Enfore plots retuns
    if (show_plots) {
        hyp_plot_args[["return_plots"]] = TRUE
        hyp_plot_args[["show_plots"]] = FALSE
    }
    if (show_emaps) {
        hyp_emap_args[["return_plots"]] = TRUE
        hyp_emap_args[["show_plots"]] = FALSE
    }    
    if (show_hmaps) {
        hyp_hmap_args[["return_plots"]] = TRUE
        hyp_hmap_args[["show_plots"]] = FALSE
    }
    
    # Markdown configuration
    if (!is.null(custom_rmd_config)) {
        rmd_config <- custom_rmd_config
    } else {
        rmd_config <- format_str(rmd_config, title, subtitle, author)
    }

    write(rmd_config, file = file_path, append = FALSE)
    write(rmd_knitr, file = file_path, append = TRUE)

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
    if (is(hyp.obj, "hyp")) {
        tabsets <- list(x = list(" " = hyp.obj))
        names(tabsets) <- c(header)
    }
    if (is(hyp.obj, "multihyp")) {
        tabsets <- list(x = hyp.obj$data)
        names(tabsets) <- c(header)
    }
    if (is(hyp.obj, "list")) {
        tabsets <- lapply(hyp.obj, function(x) {
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

    for (tabset in names(tabsets)) {

        # Tabset header and init code
        rmd_tabset %>%
        format_str(tabset) %>%
        write(file=file_path, append=TRUE)

        # Tab content
        tabs <- tabsets[[tabset]]
        for (tab in names(tabs)) {

            tab_id <- sample(1:100000000000, 1)
            
            plot_area <- ifelse(show_plots, format_str(tab_plot, string_args(hyp_plot_args)), "")
            emap_area <- ifelse(show_emaps, format_str(tab_emap, string_args(hyp_emap_args)), "")
            hmap_area <- ifelse(show_hmaps, format_str(tab_hmap, string_args(hyp_hmap_args)), "")
            table_area <- ifelse(show_tables, tab_table, "")

            rmd_tab %>%
            format_str(tab, tab_id, tabset, plot_area, emap_area, hmap_area, table_area) %>%
            write(file = file_path, append=TRUE)
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
