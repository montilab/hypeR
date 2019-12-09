hyp_dots_tests <- function(hyp_obj, return_obj=FALSE) {
    expect_silent(hyp_dots(hyp_obj))
    expect_silent(hyp_dots(hyp_obj, val="pval"))
    expect_silent(hyp_dots(hyp_obj, val="fdr"))
    expect_silent(hyp_dots(hyp_obj, abrv=20))
    expect_silent(hyp_dots(hyp_obj, title="title"))
    expect_silent(hyp_dots(hyp_obj, size=TRUE))
    p <- hyp_dots(hyp_obj, "gg")
    expect_is(p, "gg")
    if (return_obj) return(hyp_obj)
}

test_that("hyp_dots() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets_obj <- testdat$gsets
    rgsets_obj <- testdat$rgsets
    
    # Overrepresentation (signature)
    signature <- testdat$signature
    experiment <- testdat$experiment
    
    hypeR(signature, gsets_obj, test="hypergeometric", background=100) %>%
    hyp_dots_tests()
    hypeR(signature, rgsets_obj, test="hypergeometric", background=100) %>%
    hyp_dots_tests()
    p <- hypeR(experiment, gsets_obj, test="hypergeometric", background=100) %>%
         hyp_dots()
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "gg")
    
    # Enrichment (ranked signature)
    signature <- names(testdat$weighted_signature)
    experiment <- lapply(testdat$weighted_experiment, names)

    hypeR(signature, gsets_obj, test="kstest") %>%
    hyp_dots_tests()
    hypeR(signature, rgsets_obj, test="kstest") %>%
    hyp_dots_tests()
    p <- hypeR(experiment, gsets_obj, test="kstest") %>%
         hyp_dots()
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "gg")
        
    # Enrichment (weighted signature)
    signature <- testdat$weighted_signature
    experiment <- testdat$weighted_experiment
    
    hypeR(signature, gsets_obj, test="kstest") %>%
    hyp_dots_tests()
    hypeR(signature, rgsets_obj, test="kstest") %>%
    hyp_dots_tests()
    p <- hypeR(experiment, gsets_obj, test="kstest") %>%
         hyp_dots()
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "gg")
})
