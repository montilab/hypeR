library(hypeR)

#### Steps
# 1. Download the most recent version of msigdb gene sets
# 2. Separate each gene set by category and subcategory
# 3. Save each gene set as an rds object in inst/extdata
download.msigdb(species="Homo sapiens")
