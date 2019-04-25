#' Format a string using placeholders
#'
#' @param string A an unformatted string with placeholders (e.g. "Format with {1} and {2}")
#' @param ... Variables to format placeholders with
#' @return A formatted string
#'
format_str <- function(string, ...) {
    args <- list(...)
    for (i in 1:length(args)) {
        pattern <- paste("\\{", i, "}", sep="")
        replacement <- args[[i]]
        string <- gsub(pattern, replacement, string)
    }
    return(string)
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
```{r {2}, echo = FALSE}
hyp.obj <- tabsets[['{3}']][['{1}']] 
{4}
{5}
{6}
```
"

tab_plot <- "
hyp.obj %>%
hyp_plot(top={1}, val='{2}', show_plots=FALSE, return_plots=TRUE)
"

tab_emap <- "
hyp.obj %>%
hyp_emap(top={1}, val='{2}', similarity_metric='{3}', similarity_cutoff={4}, show_plots=FALSE, return_plots=TRUE)
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
#' @param show_emaps Option to show emaps in tabs
#' @param show_tables Option to show table in tabs
#' @param top_plot Limit number of pathways shown in plots
#' @param top_emap Limit number of pathways shown in emaps
#' @param val_plot Choose significance value in plots e.g. c("fdr", "pval")
#' @param val_emap Choose significance value in emaps e.g. c("fdr", "pval")
#' @param similarity_metric Similarity matric used in emaps
#' @param similarity_cutoff Similarity cutoff used in emaps
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
                       show_tables=TRUE,
                       top_plot=15,
                       top_emap=15,
                       val_plot=c("fdr", "pval"),
                       val_emap=c("fdr", "pval"),
                       similarity_metric=c("jaccard_similarity", "overlap_similarity"),
                       similarity_cutoff=0.2,
                       custom_rmd_config=NULL,
                       custom_pre_content=NULL,
                       custom_post_content=NULL) {

    # Default arguments
    val_plot <- match.arg(val_plot)
    val_emap <- match.arg(val_emap)
    similarity_metric <- match.arg(similarity_metric)

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
    if ("hyp" %in% class(hyp.obj)) {
        tabsets <- list(x = list(" " = hyp.obj))
        names(tabsets) <- c(header)
    }
    if ("multihyp" %in% class(hyp.obj)) {
        tabsets <- list(x = hyp.obj$data)
        names(tabsets) <- c(header)
    }
    if (class(hyp.obj) == "list") {
        tabsets <- lapply(hyp.obj, function(x) {
            stopifnot("hyp" %in% class(x) | "multihyp" %in% class(x))
            if ("hyp" %in% class(x)) {
                return(list(" " = x))
            }
            if ("multihyp" %in% class(x)) {
                return(x$data)
            }
        })
    }
    # ----------------------

    for (tabset in names(tabsets)) {

        # Tabset header and init code
        rmd_tabset %>%
            format_str(tabset) %>%
            write(file = file_path, append = TRUE)

        # Tab content
        tabs <- tabsets[[tabset]]
        for (tab in names(tabs)) {

            tab_id <- sample(1:100000000, 1)
            plot_area <- ifelse(show_plots, format_str(tab_plot, top_plot, val_plot), "")
            emap_area <- ifelse(show_emaps, format_str(tab_emap, top_emap, val_emap, similarity_metric, similarity_cutoff), "") 
            table_area <- ifelse(show_tables, tab_table, "")

            rmd_tab %>%
                format_str(tab, tab_id, tabset, plot_area, emap_area, table_area) %>%
                write(file = file_path, append = TRUE)
        }
    }

    # Content after hyper enrichment tabs
    if (!is.null(custom_post_content)) {
        write(custom_post_content, file = file_path, append = TRUE)
    }

    rmarkdown::render(input=file_path, 
                      output_format="html_document",
                      output_file=paste(file_path, "html", sep="."))
}
