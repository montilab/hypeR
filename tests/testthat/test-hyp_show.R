test_that("hyp_show() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets <- testdat$gsets
    rgsets <- testdat$rgsets
    signature <- testdat$signature
    experiment <- testdat$experiment
    
    hyp_obj <- hypeR(signature, gsets)
    multihyp_obj <- hypeR(experiment, gsets)

    expect_silent(hyp_show(hyp_obj))
    expect_silent(hyp_show(hyp_obj, simple=TRUE))
    expect_error(hyp_show(multihyp_obj))
})
