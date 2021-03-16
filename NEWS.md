# hypeR 1.07.03
* Version bump for bioconductor

# hypeR 1.05.03
* `rctbl_build()` now wraps `hyp` objects into unlabled `multihyp` objects
* `rctbl_build()` nested tables shows the number of enriched genesets

# hypeR 1.05.02
* Correct file extensions (.rmd/.html) from output of `hyp_to_rmd()`
* Relative paths are now supported by `hyp_to_rmd()`

# hypeR 1.05.01
* Fixed bug for wrong column names from `enrichr_available()`
* Added the first shiny module for geneset selection

# hypeR 1.05.00
* Version bump for bioconductor
* Fixed `hyp_dots(merge=TRUE)` bug where some genesets were not showing
* Added support for fetching non-human Enrichr libraries (e.g. Yeast, Fly, Worm, Fish)
* Better reporting through `rctbl_hyp()` and `rctbl_mhyp()`

# hypeR 1.04.00
* Version bump for bioconductor

# hypeR 1.03.01
* Ability to clean genesets names
* Multiple signatures can now be merged into a single plot with `hyp_dots(merge=TRUE)`
* Set the `hyp_dots()` legend title to the significance measure used
* Versioning information used in export functions now includes parameters passed to `hypeR()`

# hypeR 1.03.00
* Version bump for bioconductor

# hypeR 1.02.00
* Added the genesets object with versioning
* Downloaded genesets are now wrapped into gsets or rgsets objects automatically
* Added functionality to download genesets directly from enrichr
* Simplified functionality to download genesets via msigdbr
* Genesets versioning is included by default in exporting/reporting functions
* Tables now include the size of signatures, genesets, and the background distribution
* Converted `lapply()` functions to `mapply()`
* Removed option to disable titles when multiple signatures are plotted
* Removed option to show/return plots
* Some variable names across functions were made shorter or more clear
* Version bump for bioconductor

# hypeR 1.01.10
* Changed default order of elements under each tab in `hyp_to_rmd()` function

# hypeR 1.01.09
* Published in Bioinformatics
* https://doi.org/10.1093/bioinformatics/btz700
