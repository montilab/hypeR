% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{clean_genesets}
\alias{clean_genesets}
\title{Clean labels of genesets}
\usage{
clean_genesets(x)
}
\arguments{
\item{x}{A vector of labels}
}
\description{
Clean labels of genesets
}
\examples{
HALLMARK <- msigdb_download("Homo sapiens", "H")
names(HALLMARK) <- clean_genesets(names(HALLMARK))
head(names(HALLMARK))

}
