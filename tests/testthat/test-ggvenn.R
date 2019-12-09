test_that("ggvenn() is working", {
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    genesets <- testdat$gsets$genesets
    p <- ggvenn(signature, genesets[[1]], "signature", "genset", "Geneset Name")
    expect_is(p, "gg")
})
