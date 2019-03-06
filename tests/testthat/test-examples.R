# Grab all curated genesets
BIOCARTA <- ex_get("C2.CP.BIOCARTA")
KEGG <- ex_get("C2.CP.KEGG")
REACTOME <- ex_get("C2.CP.REACTOME")

# Genes involed in tricarboxylic acid cycle
symbols <- c("IDH3B","DLST","PCK2","CS","PDHB","PCK1","PDHA1","LOC642502",
             "PDHA2","LOC283398","FH","SDHD","OGDH","SDHB","IDH3A","SDHC",
             "IDH2","IDH1","OGDHL","PC","SDHA","SUCLG1","SUCLA2","SUCLG2")

# Hyper enrichment
hyp <- hypeR(symbols, REACTOME, bg=2520, fdr=0.05)

# Plotting
p <- hyp_plot(hyp, val="fdr")

test_that("Database retrieval is working", {
    expect_true(startsWith(names(BIOCARTA)[1], "BIOCARTA"))
    expect_true(startsWith(names(KEGG)[1], "KEGG"))
    expect_true(startsWith(names(REACTOME)[1], "REACTOME"))
})

test_that("Hyper enrichment is working", {
    expect_equal(hyp$pval, c(3.7e-28, 1.3e-26, 1.0e-18))
    expect_equal(hyp$fdr, c(2.5e-25, 4.3e-24, 2.2e-16))
})

test_that("Plotting visualization is working", {
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")
})

test_that("Hyper Dataframe can be saved as table", {
    hyp_to_table(hyp, file.path="pathways.txt")
    expect_true(file.exists("pathways.txt"))
    df <- read.table("pathways.txt", header=TRUE)
    expect_equal(df$pval, c(3.7e-28, 1.3e-26, 1.0e-18))
    expect_equal(df$fdr, c(2.5e-25, 4.3e-24, 2.2e-16))
})

test_that("Hyper Dataframe can be saved as excel", {
    hyp_to_excel(hyp, file.path="pathways.xlsx")
    expect_true(file.exists("pathways.xlsx"))
})
