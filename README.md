# hypeR
Geneset enrichment analysis based on hyper-geometric test

```R
library(devtools)

# Will change to montilab soon
devtools::install_github("anfederico/hypeR")
```

```R
library(hypeR)

# A small vector of genes involed in the Hippo Pathway
symbols <- c("LATS1", "LATS2", "YAP1", "TEAD4", "TEAD2", "WWTR1")

# Gensets available
db.info()

# Grab all curated genesets
C2 <- db.get("C2")

df <- hypeR(symbols, C2, bg=1504, fdr=0.05)

head(df)
```