
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hypeR

[![](https://img.shields.io/badge/bioconductor-3.9-3a6378.svg)](https://doi.org/doi:10.18129/B9.bioc.hypeR)
[![](https://img.shields.io/badge/platforms-linux%20%7C%20osx%20%7C%20win-2a89a1.svg)](https://bioconductor.org/checkResults/3.9/bioc-LATEST/hypeR/)
[![](https://img.shields.io/badge/lifecycle-maturing-4ba598.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://bioconductor.org/shields/build/devel/bioc/hypeR.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/hypeR/)
[![](https://img.shields.io/github/last-commit/montilab/hypeR.svg)](https://github.com/montilab/hypeR/commits/master)

## Documentation

Please visit <https://montilab.github.io/hypeR-docs/>

## Requirements

**hypeR** currently requires the latest version of R (\>= 3.6.0) to be
installed directly from Github or Bioconductor. To install with R (\>=
3.5.0) see below. Use with R (\< 3.5.0) is not recommended.

## Installation

Install the development version of the package from Github.

``` r
devtools::install_github("montilab/hypeR")
```

Or install the development version of the package from Bioconductor.

``` r
BiocManager::install("montilab/hypeR", version='devel')
```

Or install with Conda.

``` bash
conda create --name hyper
source activate hyper
conda install -c r r-devtools
R
library(devtools)
devtools::install_github("montilab/hypeR")
```

Install with previous versions of R.

``` bash
git clone https://github.com/montilab/hypeR
nano hypeR/DESCRIPTION
# Change Line 8
# Depends: R (>= 3.6.0) -> Depends: R (>= 3.5.0)
R
install.packages("path/to/hypeR", repos=NULL, type="source")
```

## Usage

``` r
library(hypeR)
```

## Terminology

### Signature

**hypeR** employs multiple types of enrichment analyses
(e.g. hypergeometric, kstest, gsea). Depending on the type, different
kinds of signatures are expected. There are three types of signatures
`hypeR()` expects.

``` r
# Simply a character vector of symbols (hypergeometric)
signature <- c("GENE1", "GENE2", "GENE3")

# A pre-ranked character vector of symbols (kstest)
ranked.signature <-  c("GENE1", "GENE2", "GENE3")

# A pre-ranked named numerical vector of symbols with ranking weights (gsea)
weighted.signature <-  c("GENE1"=1.22, "GENE2"=0.94, "GENE3"=0.77)
```

### Geneset

A geneset is simply a list of vectors, therefore, one can use any custom
geneset in their analyses, as long as it’s appropriately defined.

``` r
genesets <- list("GSET1" = c("GENE1", "GENE2", "GENE3"),
                 "GSET2" = c("GENE4", "GENE5", "GENE6"),
                 "GSET3" = c("GENE7", "GENE8", "GENE9"))
```

#### Hyper enrichment

All workflows begin with performing hyper enrichment with `hyper()`.
Often we are just interested in a single signature, as described above.
In this case, `hyper()` will return a `hyp` object. This object contains
relevant information to the enrichment results and is recognized by
downstream methods.

``` r
hyp_obj <- hypeR(signature, genesets)
```

#### Downstream methods

Please visit the [documentation](https://montilab.github.io/hypeR-docs/)
for detailed functionality. Below is a brief list of some methods.

##### Downloading genesets

``` r
# Download genesets from msigdb
msigdb_path <- msigdb_download_all(species="Homo sapiens")

BIOCARTA <- msigdb_fetch(msigdb_path, "C2.CP.BIOCARTA")
KEGG     <- msigdb_fetch(msigdb_path, "C2.CP.KEGG")
REACTOME <- msigdb_fetch(msigdb_path, "C2.CP.REACTOME")
```

##### Visualize results

``` r
# Show interactive table
hyp_show(hyp_obj)

# Plot dots plot
hyp_dots(hyp_obj)

# Plot enrichment map
hyp_emap(hyp_obj)

# Plot hiearchy map
hyp_hmap(hyp_obj)
```

##### Saving results

``` r
# Save to excel
hyp_to_excel(hyp_obj, file_path="hyper.xlsx")

# Save to table
hyp_to_table(hyp_obj, file_path="hyper.txt")

# Generate markdown report
hyp_to_rmd(lmultihyp_obj,
           file_path="hyper-enrichment.rmd",
           title="Hyper Enrichment (hypeR)",
           subtitle="YAP, TNF, and TAZ Knockout Experiments",
           author="Anthony Federico, Stefano Monti",
           show_plots=T,
           show_emaps=T,
           show_hmaps=T,
           show_tables=T)
```

## Related Repositories

  - [hypeR-db](https://github.com/montilab/hypeR-db) *A repository for
    commonly used open source genesets used by hypeR*
  - [hypeR-shiny](https://github.com/montilab/hypeR-shiny) *An example
    shiny application using hypeR*
  - [hypeR-docs](https://github.com/montilab/hypeR-docs) *Landing site
    for hosting documentation for hypeR*
