library(magrittr)
library(qusage)
library(dplyr)

# Download data -- 

# All data downloaded from https://reactome.org/download-data
gsets.url <- "https://reactome.org/download/current/ReactomePathways.gmt.zip"
nodes.url <- "https://reactome.org/download/current/ReactomePathways.txt"
edges.url <- "https://reactome.org/download/current/ReactomePathwaysRelation.txt"

# Gsets
gsets.tmp <- tempfile(fileext=".gmt.zip")
download.file(gsets.url, destfile = gsets.tmp, mode = "wb")
gsets.raw <- read.gmt(unzip(gsets.tmp))

# Nodes
nodes.raw <- read.delim(nodes.url, sep="\t", header=F, fill=T, col.names=c("id", "label", "species"), stringsAsFactors=F)

# Edges
edges.raw <- read.delim(edges.url, sep="\t", header=F, fill=T, col.names=c("from", "to"), stringsAsFactors=F)
# --

# Clean data -- 

# Species-specific nodes
nodes <- nodes.raw %>%
         filter( label %in% names(gsets.raw) ) %>%
         filter( species == "Homo sapiens" ) %>%
         filter(! duplicated(id) ) %>%
         set_rownames( .$id ) %>%
         { .[, "label", drop=F] }

# Species-specific edges
edges <- edges.raw %>%
         filter( from %in% rownames(nodes) ) %>%
         filter( to %in% rownames(nodes) )

# Leaf genesets
gsets <- nodes %>%
         rownames() %>%
         .[! . %in% edges$from] %>%
         sapply( function(x) nodes[x, "label"] ) %>%
         gsets.raw[.]
# -- 

rgsets.obj <- hypeR::rgsets$new(gsets, nodes, edges)
rgsets.lst <- list("REACTOME"=rgsets.obj)
    
saveRDS(rgsets.lst, file.path(system.file("extdata", package="hypeR"), "rgsets.rds"))
