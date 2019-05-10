test_that("hyp_emap() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    gsets <- testdat$gsets    

    hyp_obj <- hypeR(signature, gsets, bg=100)
    multihyp_obj <- hypeR(experiment, gsets, bg=100)
    
    # Handle a hyp object
    expect_silent(hyp_emap(hyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, val="pval", top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, val="fdr", top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="jaccard_similarity", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="overlap_similarity", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_emap(hyp_obj,  top=10, show_plots=FALSE, return_plots=TRUE)
    expect_is(p, "visNetwork")
    expect_is(p, "htmlwidget")
    
    # Empty results
    p <- hyp_emap(hyp_obj,  top=0, show_plots=FALSE, return_plots=TRUE)
    expect_is(p, "gg")

    ranked_signature <- testdat$ranked_signature$ranked
    hyp_obj <- hypeR(ranked_signature, gsets, test="kstest", bg=100)
    
    # Handle a hyp object
    expect_silent(hyp_emap(hyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, val="pval", top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, val="fdr", top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="jaccard_similarity", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="overlap_similarity", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_emap(hyp_obj,  top=10, show_plots=FALSE, return_plots=TRUE)
    expect_is(p, "visNetwork")
    expect_is(p, "htmlwidget")
    
    # Empty results
    p <- hyp_emap(hyp_obj,  top=0, show_plots=FALSE, return_plots=TRUE)
    expect_is(p, "gg")

    # Handle a multihyp object
    expect_silent(hyp_emap(multihyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(multihyp_obj, val="pval", top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(multihyp_obj, val="fdr",  top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(multihyp_obj, similarity_metric="jaccard_similarity", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(multihyp_obj, similarity_metric="overlap_similarity", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_emap(multihyp_obj, top=30, show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "visNetwork")
    expect_is(p[["Signature 3"]], "htmlwidget")
    
    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    multihyp_obj <- hypeR(experiment, rgsets_obj, is_rgsets=TRUE, bg=100, pval_cutoff=0.05)
    p <- hyp_emap(multihyp_obj, show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "visNetwork")
    expect_is(p[["Signature 3"]], "htmlwidget")
})
