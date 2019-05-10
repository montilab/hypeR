hyp_tests <- function(hyp_obj, test_plots=FALSE, return_obj=FALSE) {
    expect_is(hyp_obj, "hyp")
    expect_is(hyp_obj, "R6")  
    expect_is(hyp_obj$data, "data.frame")
    expect_equal(ncol(hyp_obj$data), 6)
    expect_is(hyp_obj$args, "list")
    expect_output(print(hyp_obj))
    if (test_plots) {
        expect_is(hyp_obj$plots, "list")
        expect_is(hyp_obj$plots[[1]], "gg")
        expect_equal(names(hyp_obj$plots), hyp_obj$data$label)
    }
    if (return_obj) return(hyp_obj)
}

hypeR_tests <- function(test, signature, experiment, gsets, rgsets) {
        
    # Basic
    hypeR(signature, gsets, test=test, bg=2520) %>%
    hyp_tests()
    
    # Basic with plots
    hypeR(signature, gsets, test=test, bg=2520, do_plots=TRUE) %>%
    hyp_tests(test_plots=TRUE)
    
    # Test pval_cutoff
    hypeR(signature, gsets, test=test, bg=100, pval_cutoff=0.0001) %>%
    hyp_tests()
    
    # Test fdr_cutoff
    hyp_obj <- hypeR(signature, gsets, test=test, bg=100, fdr_cutoff=0.0001) %>%
               hyp_tests(return_obj=TRUE)
    
    expect_equal(hyp_obj$args$gsets, gsets)
    expect_equal(hyp_obj$args$pval_cutoff, 1)
    expect_equal(hyp_obj$args$fdr_cutoff, 0.0001)
    
    # Test relational gsets
    hyp_obj <- hypeR(signature, rgsets, test=test, bg=80, pval_cutoff=0.01) %>%
               hyp_tests(return_obj=TRUE)
    expect_is(hyp_obj$args$gsets, "rgsets")
    expect_is(hyp_obj$args$gsets, "R6")
    
    # Multiple signatures
    multihyp_obj <- hypeR(experiment, gsets, test=test, bg=100, fdr_cutoff=0.01)
    expect_is(multihyp_obj, "multihyp")
    expect_is(multihyp_obj, "R6")
    expect_equal(names(multihyp_obj$data), c("Signature 1", "Signature 2", "Signature 3"))
    
    # Extracting hyp objects
    multihyp_obj$data[["Signature 1"]] %>%
    hyp_tests()
    
    # Extracting hyp objects with plots
    multihyp_obj <- hypeR(experiment, gsets, test=test, bg=100, do_plots=TRUE)
    multihyp_obj$data[["Signature 1"]] %>%
    hyp_tests(test_plots=TRUE)
    
    # Test relational gsets
    multihyp_obj <- hypeR(experiment, rgsets, test=test, bg=100, pval_cutoff=0.05)
    multihyp_obj$data[["Signature 2"]] %>%
    hyp_tests()
}

test_that("hypeR() is working", {
    # Testing data
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets <- testdat$gsets
    rgsets <- testdat$rgsets
    
    # Overrepresentation (signature)
    signature <- testdat$signature
    experiment <- testdat$experiment
    hypeR_tests(test="hypergeometric", signature, experiment, gsets, rgsets)
    
    # Enrichment (ranked signature)
    signature <- names(testdat$weighted_signature)
    experiment <- lapply(testdat$weighted_experiment, names)
    hypeR_tests(test="kstest", signature, experiment, gsets, rgsets)
    
    # Enrichment (weighted signature)
    signature <- testdat$weighted_signature
    experiment <- testdat$weighted_experiment
    hypeR_tests(test="kstest", signature, experiment, gsets, rgsets)
})
