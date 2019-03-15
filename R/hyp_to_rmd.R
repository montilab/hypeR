rmd_config <- "---
title: '{1}'
subtitle: '{2}'
author: '{3}'
date: 'Last Modified: `r Sys.Date()`'
output:
  html_document:
    theme: united
    toc: yes
  html_notebook:
    toc: yes
---
"

rmd_knitr <- "
```{r setup, echo=FALSE}
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
library(kableExtra)
```
"
rmd_tabset <- "
# {1}
##  {.tabset .tabset-fade}
"

rmd_tab <- "
### {2} 
```{r {5}, echo = FALSE}
hyp <- tabsets[['{1}']][['{2}']] 
{3}
{4}
```
"

tab_plot <- "
hyp %>%
hyp_plot(top={1}, val='{2}', show_plots=FALSE, return_plots=TRUE)
"

tab_table <- "
hyp %>%
as('data.frame') %>%
knitr::kable(format='html') %>% 
kable_styling() %>% 
scroll_box(height='500px', width='900px')
"

#' Export hyp object to rmarkdown
#'
#' @param hyp A hyper object, multihyp object, or list of multihyp objects
#' @param file_path Output file path
#' @param title Title of markdown report
#' @param subtitle Subtitle of markdown report
#' @param author Authors of markdown report
#' @param header Header name of tabset section
#' @param show_plots Option to show plots in tabs
#' @param show_tables Option to show table in tabs
#' @param top Limit number of pathways shown in plots
#' @param val Choose significance value in plots e.g. c("pval", "fdr")
#' @param custom_rmd_config Replace configuration section of markdown report
#' @param custom_pre_content Insert custom content before tabset section
#' @param custom_post_content Insert custom content after tabset section
#' @return None
#'
#' @importFrom rmarkdown render
#' @importFrom magrittr %>%
#' @export
hyp_to_rmd <- function(hyp, 
                       file_path, 
                       title="",
                       subtitle="",
                       author="",
                       header="Hyper Enrichment",
                       show_plots=TRUE,
                       show_tables=TRUE,
                       top=15,
                       val="fdr",
                       custom_rmd_config=NULL,
                       custom_pre_content=NULL,
                       custom_post_content=NULL) {
    
    # Markdown configuration
    if (!is.null(custom_rmd_config)) {
        rmd_config <- custom_rmd_config
    } else {
        rmd_config <- rmd_config %>%
                      gsub('\\{1}', title, .) %>%
                      gsub('\\{2}', subtitle, .) %>%
                      gsub('\\{3}', author, .)
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
    if (class(hyp) == "hyp") {
        tabsets <- list(header = list(" " = hyp))
    }
    if (class(hyp) == "multihyp") {
        tabsets <- list(header = hyp@data)
    }
    if (class(hyp) == "list") {
        tabsets <- lapply(hyp, function(x) {
            stopifnot(class(x) == "hyp" | class(x) == "multihyp")
            if (class(x) == "hyp") {
                return(list(" " = x))
            }
            if (class(x) == "multihyp") {
                return(x@data)
            }
        })
    }
    # ----------------------

    for (tabset in names(tabsets)) {

        # Tabset header and init code
        rmd_tabset %>%
            gsub('\\{1}', tabset, .) %>%
            write(file = file_path, append = TRUE)

        # Tab content
        tabs <- tabsets[[tabset]]
        for (tab in names(tabs)) {

            rmd_tab %>%
                gsub("\\{1}", tabset, .) %>%
                gsub("\\{2}", tab, .) %>%
                gsub("\\{5}", sample(1:100000000, 1), .) %>%
                { if (show_plots) {
                      gsub("\\{3}", tab_plot %>%
                                    gsub('\\{1}', top, .) %>%
                                    gsub('\\{2}', val, .), .)
                   } else { . }
                } %>%
                { ifelse(show_tables, gsub("\\{4}", tab_table, .), .) } %>%
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
