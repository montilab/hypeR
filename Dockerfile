FROM bioconductor/bioconductor_docker:devel

MAINTAINER Anthony Federico <anfed@bu.edu>

RUN Rscript -e \
    'install.packages("ggplot2"); \
     install.packages("ggforce"); \
     install.packages("R6"); \
     install.packages("magrittr"); \
     install.packages("dplyr"); \
     install.packages("purrr"); \
     install.packages("stringr"); \
     install.packages("scales"); \
     install.packages("httr"); \
     install.packages("openxlsx"); \
     install.packages("reshape2"); \
     install.packages("reactable"); \
     install.packages("msigdbr"); \
     install.packages("kableExtra"); \
     install.packages("rmarkdown"); \
     install.packages("igraph"); \
     install.packages("visNetwork"); \
     install.packages("BiocManager"); \
     BiocManager::install("BiocStyles"); \
     BiocManager::install("BiocCheck");'
