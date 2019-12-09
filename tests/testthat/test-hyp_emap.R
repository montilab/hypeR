hyp_emap_tests <- function(hyp_obj, return_obj=FALSE) {
    expect_silent(hyp_emap(hyp_obj, top=5))
    expect_silent(hyp_emap(hyp_obj, val="pval", top=10))
    expect_silent(hyp_emap(hyp_obj, val="fdr", top=10))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="jaccard_similarity"))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="overlap_similarity"))
    p <- hyp_emap(hyp_obj,  top=10)
    expect_is(p, "visNetwork")
    expect_is(p, "htmlwidget")
    p <- hyp_emap(hyp_obj,  top=0)
    expect_is(p, "gg")
    if (return_obj) return(hyp_obj)
}

test_that("hyp_emap() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets_obj <- testdat$gsets
    rgsets_obj <- testdat$rgsets
    
    # Overrepresentation (signature)
    signature <- testdat$signature
    experiment <- testdat$experiment
    
    hypeR(signature, gsets_obj, test="hypergeometric", background=100) %>%
    hyp_emap_tests()
    hypeR(signature, rgsets_obj, test="hypergeometric", background=100) %>%
    hyp_emap_tests()
    p <- hypeR(experiment, gsets_obj, test="hypergeometric", background=100) %>%
         hyp_emap()
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "visNetwork")
    
    # Enrichment (ranked signature)
    signature <- names(testdat$weighted_signature)
    experiment <- lapply(testdat$weighted_experiment, names)

    hypeR(signature, gsets_obj, test="kstest") %>%
    hyp_emap_tests()
    hypeR(signature, rgsets_obj, test="kstest") %>%
    hyp_emap_tests()
    p <- hypeR(experiment, gsets_obj, test="kstest") %>%
         hyp_emap()
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "visNetwork")
        
    # Enrichment (weighted signature)
    signature <- testdat$weighted_signature
    experiment <- testdat$weighted_experiment
    
    hypeR(signature, gsets_obj, test="kstest") %>%
    hyp_emap_tests()
    hypeR(signature, rgsets_obj, test="kstest") %>%
    hyp_emap_tests()
    p <- hypeR(experiment, gsets_obj, test="kstest") %>%
         hyp_emap()
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "visNetwork")
})
