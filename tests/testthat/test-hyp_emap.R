hyp_emap_tests <- function(hyp_obj, return_obj=FALSE) {
    expect_silent(hyp_emap(hyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, val="pval", top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, val="fdr", top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="jaccard_similarity", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="overlap_similarity", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_emap(hyp_obj,  top=10, show_plots=FALSE, return_plots=TRUE)
    expect_is(p, "visNetwork")
    expect_is(p, "htmlwidget")
    p <- hyp_emap(hyp_obj,  top=0, show_plots=FALSE, return_plots=TRUE)
    expect_is(p, "gg")
    if (return_obj) return(hyp_obj)
}

test_that("hyp_emap() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets <- testdat$gsets
    rgsets <- testdat$rgsets
    
    # Overrepresentation (signature)
    signature <- testdat$signature
    experiment <- testdat$experiment
    
    hypeR(signature, gsets, test="hypergeometric", bg=100) %>%
    hyp_emap_tests()
    hypeR(signature, rgsets, test="hypergeometric", bg=100) %>%
    hyp_emap_tests()
    p <- hypeR(experiment, gsets, test="hypergeometric", bg=100) %>%
         hyp_emap(show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "visNetwork")
    
    # Enrichment (ranked signature)
    signature <- names(testdat$weighted_signature)
    experiment <- lapply(testdat$weighted_experiment, names)

    hypeR(signature, gsets, test="kstest") %>%
    hyp_emap_tests()
    hypeR(signature, rgsets, test="kstest") %>%
    hyp_emap_tests()
    p <- hypeR(experiment, gsets, test="kstest") %>%
         hyp_emap(show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "visNetwork")
        
    # Enrichment (weighted signature)
    signature <- testdat$weighted_signature
    experiment <- testdat$weighted_experiment
    
    hypeR(signature, gsets, test="kstest") %>%
    hyp_emap_tests()
    hypeR(signature, rgsets, test="kstest") %>%
    hyp_emap_tests()
    p <- hypeR(experiment, gsets, test="kstest") %>%
         hyp_emap(show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "visNetwork")
})
