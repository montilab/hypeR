# hypeR
Geneset enrichment analysis based on hyper-geometric test

### Install
```R
library(devtools)

devtools::install_github("montilab/hypeR")
```

### Usage
```R
library(hypeR)

# Genes involed in tricarboxylic acid cycle
symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
             "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
             "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")

# Gensets available
db.info()

# Grab all curated genesets
C2 <- db.get("C2")

# Perform hyper enrichment
hyp <- hypeR(symbols, C2, bg=2522, fdr=0.05)

# Interactive table
hyp.show(hyp)

# Save enriched pathways
hyp.to.excel(hyp, file.path="pathways.xlsx")

# Visualize
hyp.plot(hyp, top=13)
```