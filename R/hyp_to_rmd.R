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
### {1} 
```{r {1}, echo = FALSE}
df <- hyp[['{1}']] 
{2}
{3}
```
"

tab_plot <- "
df %>%
hyp_plot(top={1}, val='{2}', show_plots=FALSE, return_plots=TRUE)
"

tab_table <- "
df %>%
knitr::kable(format='html') %>% 
kable_styling() %>% 
scroll_box(height='500px', width='900px')
"

#' Export hyper object to rmarkdown
#'
#' @param hyp A hyper object
#' @param file.path Output file path
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
#' @export
hyp_to_rmd <- function(hyp, 
                       file.path, 
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

    write(rmd_config, file = file.path, append = FALSE)
    write(rmd_knitr, file = file.path, append = TRUE)

    # Content before hyper enrichment tabs
    if (!is.null(custom_pre_content)) {
        write(custom_pre_content, file = file.path, append = TRUE)
    }

    # Writing tabs
    rmd_tabset <- rmd_tabset %>% 
                  gsub('\\{1}', header, .)

    write(rmd_tabset, file = file.path, append = TRUE)

    # Handle single dataframe hyper objects
    if (class(hyp) == "data.frame") {
        hyp <- list(" " = hyp)
    }
    for (i in names(hyp)) {

        rmd_tab_i <- rmd_tab %>%
                     gsub("\\{1}", i, .) %>%
                     { if (show_plots) {
                            tab_plot <- tab_plot %>%
                                        gsub('\\{1}', top, .) %>%
                                        gsub('\\{2}', val, .)

                            # Parameters passed to plotting
                            gsub("\\{2}", 
                                tab_plot %>%
                                gsub('\\{1}', top, .) %>%
                                gsub('\\{2}', val, .), 
                                .)
                       } else {
                            .
                       }
                     } %>%
                     { ifelse(show_tables, gsub("\\{3}", tab_table, .), .) }

        write(rmd_tab_i, file = file.path, append = TRUE)
    }

    # Content after hyper enrichment tabs
    if (!is.null(custom_post_content)) {
        write(custom_post_content, file = file.path, append = TRUE)
    }

    rmarkdown::render(input=file.path, 
                      output_format="html_document",
                      output_file=paste(file.path, "html", sep="."))
}
