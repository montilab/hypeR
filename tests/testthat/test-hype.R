test_that("hypeR(test='hypergeometric') is working with single signatures", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    gsets <- testdat$gsets
    
    # Basic output
    hyp_obj <- hypeR(signature, gsets, bg=2520)
    expect_is(hyp_obj, "hyp")
    expect_is(hyp_obj, "R6")
    expect_equal(dim(hyp_obj$data), c(10, 5))
    
    # Basic output with plots
    hyp_obj <- hypeR(signature, gsets, bg=2520, do_plots=TRUE)
    expect_equal(names(hyp_obj$plots), rownames(hyp_obj$data))
    expect_is(hyp_obj$plots[[1]], "gg")
    
    # Test pval_cutoff
    hyp_obj <- hypeR(signature, gsets, bg=100, pval_cutoff=0.01)
    
    # Test fdr_cutoff
    hyp_obj <- hypeR(signature, gsets, bg=100, fdr_cutoff=0.01)

    # Test hyp object
    expect_equal(hyp_obj$args$gsets, gsets)
    expect_equal(hyp_obj$args$pval_cutoff, 1)
    expect_equal(hyp_obj$args$fdr_cutoff, 0.01)
    
    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    expect_error(hypeR(signature, rgsets_obj, bg=80, fdr_cutoff=0.01))
    hyp_obj <- hypeR(signature, rgsets_obj, is_rgset=TRUE, bg=80, pval_cutoff=0.01)
    expect_true(hyp_obj$args$is_rgset)
    expect_is(hyp_obj$args$gsets, "rgsets")
    expect_is(hyp_obj$args$gsets, "R6")
    
    # Basic output with plots with plots
    hyp_obj <- hypeR(signature, rgsets_obj, is_rgset=TRUE, bg=80, pval_cutoff=0.01, do_plots=TRUE)
    expect_equal(names(hyp_obj$plots), rownames(hyp_obj$data))
    expect_is(hyp_obj$plots[[1]], "gg")
})

test_that("hypeR(test='hypergeometric') is working with multiple signatures", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    experiment <- testdat$experiment
    gsets <- testdat$gsets    
    
    # Basic output
    multihyp_obj <- hypeR(experiment, gsets, bg=100, fdr_cutoff=0.01)
    expect_is(multihyp_obj, "multihyp")
    expect_is(multihyp_obj, "R6")
    expect_equal(names(multihyp_obj$data), c("Signature 1", "Signature 2", "Signature 3"))
    
    # Extracting hyp objects
    hyp_obj <- multihyp_obj$data[["Signature 1"]]
    expect_is(hyp_obj, "hyp")   
    expect_is(hyp_obj, "R6")   
    expect_equal(hyp_obj$args$gsets, gsets)
    expect_equal(hyp_obj$args$pval_cutoff, 1)
    expect_equal(hyp_obj$args$fdr_cutoff, 0.01)
    
    # Extracting hyp objects with plots
    multihyp_obj <- hypeR(experiment, gsets, bg=100, fdr_cutoff=0.01, do_plots=TRUE)
    hyp_obj <- multihyp_obj$data[["Signature 1"]]
    expect_equal(names(hyp_obj$plots), rownames(hyp_obj$data))
    expect_is(hyp_obj$plots[[1]], "gg")
    
    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    expect_error(hypeR(experiment, rgsets_obj, bg=100, fdr_cutoff=0.01))
    multihyp_obj <- hypeR(experiment, rgsets_obj, is_rgsets=TRUE, bg=100, pval_cutoff=0.05)
    hyp_obj <- multihyp_obj$data[["Signature 2"]]
    expect_is(hyp_obj$args$gsets, "rgsets")
    expect_is(hyp_obj$args$gsets, "R6")
})

test_that("hypeR(test='kstest') is working with single signatures", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$ranked_signature$ranked
    weights <- testdat$ranked_signature$weights
    gsets <- testdat$gsets
    
    # Basic output
    hyp_obj <- hypeR(signature, gsets, test="kstest", bg=2520)
    expect_is(hyp_obj, "hyp")
    expect_is(hyp_obj, "R6")
    expect_equal(dim(hyp_obj$data), c(10, 5))
 
    # Basic output with weights
    hyp_obj <- hypeR(signature, gsets, test="kstest", bg=2520, weights=weights)
    expect_is(hyp_obj, "hyp")
    expect_is(hyp_obj, "R6")
    expect_equal(dim(hyp_obj$data), c(10, 5))
      
    # Basic output with plots
    hyp_obj <- hypeR(signature, gsets, test="kstest", bg=2520, do_plots=TRUE)
    expect_equal(names(hyp_obj$plots), rownames(hyp_obj$data))
    expect_is(hyp_obj$plots[[1]], "gg")
    
    # Test pval_cutoff
    hyp_obj <- hypeR(signature, gsets, test="kstest", bg=100, pval_cutoff=0.01)
    
    # Test fdr_cutoff
    hyp_obj <- hypeR(signature, gsets, test="kstest", bg=100, fdr_cutoff=0.01)

    # Test hyp object
    expect_equal(hyp_obj$args$gsets, gsets)
    expect_equal(hyp_obj$args$pval_cutoff, 1)
    expect_equal(hyp_obj$args$fdr_cutoff, 0.01)
    
    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    expect_error(hypeR(signature, rgsets_obj, test="kstest", bg=80, fdr_cutoff=0.01))
    hyp_obj <- hypeR(signature, rgsets_obj, test="kstest", is_rgset=TRUE, bg=80, pval_cutoff=0.01)
    expect_true(hyp_obj$args$is_rgset)
    expect_is(hyp_obj$args$gsets, "rgsets")
    expect_is(hyp_obj$args$gsets, "R6")
})

test_that("hypeR(test='kstest') is working with multiple signatures", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    experiment <- testdat$ranked_experiment
    signature <- do.call(rbind, experiment)[,'ranked']
    weights <- do.call(rbind, experiment)[,'weights']
    gsets <- testdat$gsets    
    
    # Basic output
    multihyp_obj <- hypeR(signature, gsets, test="kstest", bg=100)
    expect_is(multihyp_obj, "multihyp")
    expect_is(multihyp_obj, "R6")
    expect_equal(names(multihyp_obj$data), c("Signature 1", "Signature 2", "Signature 3"))
    
    # Extracting hyp objects
    hyp_obj <- multihyp_obj$data[["Signature 1"]]
    expect_is(hyp_obj, "hyp")   
    expect_is(hyp_obj, "R6")   
    expect_equal(hyp_obj$args$gsets, gsets)
    expect_equal(hyp_obj$args$pval_cutoff, 1)
    expect_equal(hyp_obj$args$fdr_cutoff, 1)
    
    # Basic output with weights
    multihyp_obj <- hypeR(signature, gsets, test="kstest", weights=weights, bg=100, pval_cutoff=0.5)
    expect_is(multihyp_obj, "multihyp")
    expect_is(multihyp_obj, "R6")
    expect_equal(names(multihyp_obj$data), c("Signature 1", "Signature 2", "Signature 3"))
    
    # Extracting hyp objects with weights
    hyp_obj <- multihyp_obj$data[["Signature 1"]]
    expect_is(hyp_obj, "hyp")   
    expect_is(hyp_obj, "R6")   
    expect_equal(hyp_obj$args$signature, signature[[1]])
    expect_equal(hyp_obj$args$weights, weights[[1]])
    expect_equal(hyp_obj$args$gsets, gsets)
    expect_equal(hyp_obj$args$pval_cutoff, 0.5)
    expect_equal(hyp_obj$args$fdr_cutoff, 1)
    hyp_obj <- multihyp_obj$data[["Signature 2"]]  
    expect_equal(hyp_obj$args$signature, signature[[2]])
    expect_equal(hyp_obj$args$weights, weights[[2]])   

    # Extracting hyp objects with plots
    multihyp_obj <- hypeR(signature, gsets, test="kstest", bg=100, pval_cutoff=0.5, do_plots=TRUE)
    hyp_obj <- multihyp_obj$data[["Signature 1"]]
    expect_equal(names(hyp_obj$plots), rownames(hyp_obj$data))
    expect_is(hyp_obj$plots[[1]], "gg")
    
    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    expect_error(hypeR(signature, rgsets_obj, test="kstest", bg=100, pval_cutoff=1))
    multihyp_obj <- hypeR(signature, rgsets_obj, test="kstest", is_rgsets=TRUE, bg=100, pval_cutoff=0.5, do_plots=TRUE)
    hyp_obj <- multihyp_obj$data[["Signature 2"]]
    expect_is(hyp_obj$args$gsets, "rgsets")
    expect_is(hyp_obj$args$gsets, "R6")
    expect_is(hyp_obj$plots[[1]], "gg")
})
