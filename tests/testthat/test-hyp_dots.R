test_that("hyp_dots() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    ranked_signature <- testdat$ranked_signature$ranked
    gsets <- testdat$gsets    

    # Handle a hyp object with hypergeometric
    hyp_obj <- hypeR(signature, gsets, test="hypergeometric", bg=100)
    expect_silent(hyp_dots(hyp_obj, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_dots(hyp_obj, val="pval", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_dots(hyp_obj, val="fdr", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_dots(hyp_obj, show_plots=FALSE, return_plots=TRUE)
    expect_is(p, "gg")
    
    # Handle a hyp object with kstest
    hyp_obj <- hypeR(ranked_signature, gsets, test="kstest", bg=100)
    expect_silent(hyp_dots(hyp_obj, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_dots(hyp_obj, val="pval", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_dots(hyp_obj, val="fdr", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_dots(hyp_obj, show_plots=FALSE, return_plots=TRUE)
    expect_is(p, "gg")
    
    # Handle a multihyp object
    multihyp_obj <- hypeR(experiment, gsets, bg=100)
    expect_silent(hyp_dots(multihyp_obj, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_dots(multihyp_obj, val="pval", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_dots(multihyp_obj, val="fdr", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_dots(multihyp_obj, show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "gg")

    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    multihyp_obj <- hypeR(experiment, rgsets_obj, is_rgsets=TRUE, bg=100, pval_cutoff=0.05)
    p <- hyp_dots(multihyp_obj, show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "gg")
})
