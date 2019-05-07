test_that("hyp_hmap() is working", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    gsets <- testdat$gsets    

    # Without relational gsets
    hyp_obj <- hypeR(signature, gsets, bg=100)
    multihyp_obj <- hypeR(experiment, gsets, bg=100)
    expect_error(hyp_hmap(hyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    expect_error(hyp_hmap(multihyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    
    rgsets_obj <- testdat$rgsets
    hyp_obj <- hypeR(signature, rgsets_obj, is_rgsets=TRUE, bg=100)
    multihyp_obj <- hypeR(experiment, rgsets_obj, is_rgsets=TRUE, bg=100)
    
    # Handle a hyp object
    expect_silent(hyp_hmap(hyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_hmap(hyp_obj, val="pval", top=15, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_hmap(hyp_obj, val="fdr", top=15, show_plots=TRUE, return_plots=FALSE))
    p <- hyp_hmap(hyp_obj,  top=10, show_plots=FALSE, return_plots=TRUE)
    expect_s3_class(p, "visNetwork")
    expect_s3_class(p, "htmlwidget")
    
    # Empty results
    p <- hyp_hmap(hyp_obj,  top=0, show_plots=FALSE, return_plots=TRUE)
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")   
    expect_warning(show(p))

    ranked_signature <- testdat$ranked_signature$ranked
    hyp_obj <- hypeR(ranked_signature, rgsets_obj, test="kstest", is_rgsets=TRUE, bg=100, do_plots=TRUE)
    # Handle a hyp object
    expect_silent(hyp_hmap(hyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_hmap(hyp_obj, val="pval", top=15, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_hmap(hyp_obj, val="fdr", top=15, show_plots=TRUE, return_plots=FALSE))
    p <- hyp_hmap(hyp_obj,  top=10, show_plots=FALSE, return_plots=TRUE)
    expect_s3_class(p, "visNetwork")
    expect_s3_class(p, "htmlwidget")      
    
    # Handle a multihyp object
    expect_silent(hyp_hmap(multihyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_hmap(multihyp_obj, val="pval", top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_hmap(multihyp_obj, val="fdr",  top=10, show_plots=TRUE, return_plots=FALSE))
    p <- hyp_hmap(multihyp_obj, top=30, show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_s3_class(p[["Signature 3"]], "visNetwork")
    expect_s3_class(p[["Signature 3"]], "htmlwidget")
})
