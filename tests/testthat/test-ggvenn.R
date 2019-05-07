test_that("ggvenn() is working", {
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    gsets <- testdat$gsets
    p <- ggvenn(signature, gsets[[1]], "signature", "genset", "Geneset Name")
    expect_is(p, "gg")
})
