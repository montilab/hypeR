library(msigdbr)

msigdb_helper <- function(species="Homo sapiens", category, subcategory) {

    # Download genesets
    mdf <- msigdbr(species, category, subcategory) %>%
           dplyr::select(gs_name, gene_symbol) %>%
           as.data.frame() %>%
           stats::aggregate(gene_symbol ~ gs_name, data=., c)

    # Convert to list
    gsets <- as.list(mdf$gene_symbol)
    names(gsets) <- mdf$gs_name
    return(gsets)
}

gsets <- list(BIOCARTA=msigdb_helper(category="C2", subcategory="CP:BIOCARTA"),
              KEGG=msigdb_helper(category="C2", subcategory="CP:KEGG"),
              REACTOME=msigdb_helper(category="C2", subcategory="CP:REACTOME"))

saveRDS(gsets, file.path(system.file("extdata", package="hypeR"), "gsets.rds"))
