% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyp_to_excel.R
\name{hyp_to_excel}
\alias{hyp_to_excel}
\title{Export hyp/multihyp object to excel}
\usage{
hyp_to_excel(hyp_obj, file_path, cols = NULL, versioning = TRUE)
}
\arguments{
\item{hyp_obj}{A hyp or multihyp object}

\item{file_path}{A file path}

\item{cols}{Dataframe columns to include}

\item{versioning}{Add sheet with versioning information}
}
\description{
Export hyp/multihyp object to excel
}
\examples{
genesets <- msigdb_gsets("Homo sapiens", "C2", "CP:KEGG_LEGACY")

signature <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
               "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
               "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")

hyp_obj <- hypeR(signature, genesets, background=2522)

hyp_to_excel(hyp_obj, file_path="pathways.xlsx")

}
