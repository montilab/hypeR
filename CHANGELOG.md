## Change Log
All notable changes to this project will be documented in this file.

#### v1.2.00 - Changed
- Added the genesets object with versioning
- Downloaded genesets are now wrapped into gsets or rgsets objects automatically
- Added functionality to download genesets directly from enrichr
- Simplified functionality to download genesets via msigdbr
- Genesets versioning is included by default in exporting/reporting functions
- Tables now include the size of signatures, genesets, and the background distribution
- Converted `lapply()` functions to `mapply()`
- Removed option to disable titles when multiple signatures are plotted
- Removed option to show/return plots
- Some variable names across functions were made shorter or more clear

#### v1.1.10 - Changed
- Changed default order of elements under each tab in `hyp_to_rmd()` function.

#### v1.1.09 - Released
- Published in Bioinformatics
- https://doi.org/10.1093/bioinformatics/btz700
