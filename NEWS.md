# hypeR 1.3.01
* Multiple signatures can now be merged into a single plot with `hyp_dots(merge=TRUE)`
* Set the `hyp_dots()` legend title to the significance measure used`
* Versioning information used in export functions now includes parameters passed to `hypeR()`

# hypeR 1.3.00
* Version bump for bioconductor release 3.11

# hypeR 1.2.00
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
* Version bump for bioconductor release 3.10

# hypeR 1.1.10
* Changed default order of elements under each tab in `hyp_to_rmd()` function.

# hypeR 1.1.09
* Published in Bioinformatics
* https://doi.org/10.1093/bioinformatics/btz700
