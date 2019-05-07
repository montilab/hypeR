test_that("hyp_show() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    gsets <- testdat$gsets    
    
    hyp_obj <- hypeR(signature, gsets, test="hypergeometric", bg=100)
    multihyp_obj <- hypeR(experiment, gsets, test="hypergeometric", bg=100)
    expect_silent(hyp_show(hyp_obj))
    expect_silent(hyp_show(hyp_obj, simple=TRUE))
    expect_error(hyp_show(multihyp_obj))
    
    ranked_signature <- testdat$ranked_signature$ranked
    hyp_obj <- hypeR(ranked_signature, gsets, test="kstest", bg=100)
    expect_silent(hyp_show(hyp_obj))
    expect_silent(hyp_show(hyp_obj, simple=TRUE))
})
