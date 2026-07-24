test_that("hyp_show() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets_obj <- testdat$gsets
    rgsets_obj <- testdat$rgsets
    signature <- testdat$signature
    experiment <- testdat$experiment
    
    hyp_obj <- hypeR(signature, gsets_obj)
    kstest_obj <- hypeR(experiment[[1]], gsets_obj, test="kstest")
    multihyp_obj <- hypeR(experiment, gsets_obj)

    expect_silent(hyp_show(hyp_obj))
    expect_silent(hyp_show(hyp_obj, simple=TRUE))
    expect_equal(colnames(hyp_show(kstest_obj)$data)[1:4], c("Label", "ES", "P-Value", "FDR"))
    expect_error(hyp_show(multihyp_obj))
})
