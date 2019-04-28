test_that("hypeR() is working with single signatures", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    gsets <- testdat$gsets
    
    # Basic output
    hyp_obj <- hypeR(signature, gsets, bg=2520)
    expect_is(hyp_obj, "hyp")
    expect_is(hyp_obj, "R6")
    expect_equal(dim(hyp_obj$data), c(10, 8))
    
    # Test pval_cutoff
    hyp_obj <- hypeR(signature, gsets, bg=100, pval_cutoff=0.01)
    expect_equal(dim(hyp_obj$data), c(6, 8)) 
    expect_equal(hyp_obj$data$pval[1], 6.1e-05)
    expect_equal(hyp_obj$data$fdr[1], 0.00061)    
    
    # Test fdr_cutoff
    hyp_obj <- hypeR(signature, gsets, bg=100, fdr_cutoff=0.01)
    expect_equal(dim(hyp_obj$data), c(4, 8)) 
    expect_equal(hyp_obj$data$pval[1], c(6.1e-05))
    expect_equal(hyp_obj$data$fdr[1], c(0.00061))
    
    # Test hyp object
    expect_equal(hyp_obj$args$gsets, gsets)
    expect_equal(hyp_obj$args$pval_cutoff, 1)
    expect_equal(hyp_obj$args$fdr_cutoff, 0.01)
    
    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    expect_error(hypeR(signature, rgsets_obj, bg=80, fdr_cutoff=0.01))
    hyp_obj <- hypeR(signature, rgsets_obj, gsets_relational=TRUE, bg=80, pval_cutoff=0.01)
    expect_true(hyp_obj$args$gsets_relational)
    expect_is(hyp_obj$args$gsets, "rgsets")
    expect_is(hyp_obj$args$gsets, "R6")
    expect_equal(hyp_obj$data$pval[1], 0.00023)
    expect_equal(hyp_obj$data$fdr[1], 0.0023)
})

test_that("hypeR() is working with multiple signatures", {

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
    expect_equal(dim(hyp_obj$data), c(2, 8))
    expect_equal(hyp_obj$args$gsets, gsets)
    expect_equal(hyp_obj$args$pval_cutoff, 1)
    expect_equal(hyp_obj$args$fdr_cutoff, 0.01)
    
    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    expect_error(hypeR(experiment, rgsets_obj, bg=100, fdr_cutoff=0.01))
    multihyp_obj <- hypeR(experiment, rgsets_obj, gsets_relational=TRUE, bg=100, pval_cutoff=0.05)
    hyp_obj <- multihyp_obj$data[["Signature 2"]]
    expect_equal(dim(hyp_obj$data), c(7, 8))
    expect_is(hyp_obj$args$gsets, "rgsets")
    expect_is(hyp_obj$args$gsets, "R6")
})

test_that("hyp_show() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    gsets <- testdat$gsets    
    
    hyp_obj <- hypeR(signature, gsets, bg=100)
    multihyp_obj <- hypeR(experiment, gsets, bg=100)
    
    # Should run
    expect_silent(hyp_show(hyp_obj))
    
    # Should fail
    expect_error(hyp_show(multihyp_obj))
})

test_that("hyp_to_excel() is working", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    gsets <- testdat$gsets    
    
    hyp_obj <- hypeR(signature, gsets, bg=100)
    multihyp_obj <- hypeR(experiment, gsets, bg=100)
    
    # A single excel file with one tab
    hyp_to_excel(hyp_obj, file_path="hyp-pathways.xlsx")
    expect_true(file.exists("hyp-pathways.xlsx"))
    
    # A single excel file with multiple tabs
    hyp_to_excel(multihyp_obj, file_path="multihyp-pathways.xlsx")
    expect_true(file.exists("multihyp-pathways.xlsx"))
})

test_that("hyp_to_table() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    gsets <- testdat$gsets    

    hyp_obj <- hypeR(signature, gsets, bg=100)
    multihyp_obj <- hypeR(experiment, gsets, bg=100)
    
    # A single file
    hyp_to_table(hyp_obj, file_path="signature.txt", sep="\t")
    expect_true(file.exists("signature.txt"))
    
    # Multiple files within a directory
    hyp_to_table(multihyp_obj, file_path="experiment", sep="\t")
    expect_true(file.exists("experiment/Signature 1.txt"))
    expect_true(file.exists("experiment/Signature 2.txt"))
    expect_true(file.exists("experiment/Signature 3.txt"))
})

test_that("hyp_plot() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    gsets <- testdat$gsets    

    hyp_obj <- hypeR(signature, gsets, bg=100)
    multihyp_obj <- hypeR(experiment, gsets, bg=100)
    
    # Handle a hyp object
    expect_silent(hyp_plot(hyp_obj, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_plot(hyp_obj, val="pval", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_plot(hyp_obj, val="fdr", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_plot(hyp_obj, show_plots=FALSE, return_plots=TRUE)
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")
    
    # Handle a multihyp object
    expect_silent(hyp_plot(multihyp_obj, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_plot(multihyp_obj, val="pval", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_plot(multihyp_obj, val="fdr", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_plot(multihyp_obj, show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_s3_class(p[["Signature 3"]], "plotly")
    expect_s3_class(p[["Signature 3"]], "htmlwidget")
    
    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    multihyp_obj <- hypeR(experiment, rgsets_obj, gsets_relational=TRUE, bg=100, pval_cutoff=0.05)
    p <- hyp_plot(multihyp_obj, show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_s3_class(p[["Signature 3"]], "plotly")
    expect_s3_class(p[["Signature 3"]], "htmlwidget")
})

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
    expect_s3_class(p, "visNetwork")
    expect_s3_class(p, "htmlwidget")
    
    # Empty results
    p <- hyp_emap(hyp_obj,  top=0, show_plots=FALSE, return_plots=TRUE)
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")   
    expect_warning(show(p))
    
    # Handle a multihyp object
    expect_silent(hyp_emap(multihyp_obj, top=5, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(multihyp_obj, val="pval", top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(multihyp_obj, val="fdr",  top=10, show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(multihyp_obj, similarity_metric="jaccard_similarity", show_plots=TRUE, return_plots=FALSE))
    expect_silent(hyp_emap(multihyp_obj, similarity_metric="overlap_similarity", show_plots=TRUE, return_plots=FALSE))
    p <- hyp_emap(multihyp_obj, top=30, show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_s3_class(p[["Signature 3"]], "visNetwork")
    expect_s3_class(p[["Signature 3"]], "htmlwidget")
    
    # Test relational gsets
    rgsets_obj <- testdat$rgsets
    multihyp_obj <- hypeR(experiment, rgsets_obj, gsets_relational=TRUE, bg=100, pval_cutoff=0.05)
    p <- hyp_emap(multihyp_obj, show_plots=FALSE, return_plots=TRUE)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_s3_class(p[["Signature 3"]], "visNetwork")
    expect_s3_class(p[["Signature 3"]], "htmlwidget")
})

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
    hyp_obj <- hypeR(signature, rgsets_obj, gsets_relational=TRUE, bg=100)
    multihyp_obj <- hypeR(experiment, rgsets_obj, gsets_relational=TRUE, bg=100)
    
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

test_that("hyp_to_rmd() is working", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    project <- testdat$project
    gsets <- testdat$gsets    
    
    # Without relational gsets
    hyp_obj <- hypeR(signature, gsets, bg=100)
    multihyp_obj <- hypeR(experiment, gsets, bg=100)
    lmultihyp_obj <- lapply(project, function(x) {hypeR(x, gsets, bg=100)})
    
    # hyp
    hyp_to_rmd(hyp_obj,
               file_path="hyp.1.rmd", 
               hyp_plot_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"))
    expect_true(file.exists("hyp.1.rmd"))
    expect_true(file.exists("hyp.1.rmd.html"))    
    
    # multihyp
    hyp_to_rmd(multihyp_obj, 
               file_path="multihyp.1.rmd", 
               hyp_plot_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"))
    expect_true(file.exists("multihyp.1.rmd"))
    expect_true(file.exists("multihyp.1.rmd.html"))
    
    # multihyp
    hyp_to_rmd(lmultihyp_obj, 
               file_path="lmultihyp.1.rmd", 
               hyp_plot_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"))
    expect_true(file.exists("lmultihyp.1.rmd"))
    expect_true(file.exists("lmultihyp.1.rmd.html"))  
    
    # hyp
    expect_error(hyp_to_rmd(hyp_obj,
                            file_path="hyp.1.rmd", 
                            show_hmaps=TRUE))

    # Without relational gsets
    rgsets_obj <- testdat$rgsets
    hyp_obj <- hypeR(signature, rgsets_obj, gsets_relational=TRUE, bg=100)
    multihyp_obj <- hypeR(experiment, rgsets_obj, gsets_relational=TRUE, bg=100)
    lmultihyp_obj <- lapply(project, function(x) {hypeR(x, rgsets_obj, gsets_relational=TRUE, bg=100)})
    
    # hyp
    hyp_to_rmd(hyp_obj,
               file_path="hyp.2.rmd", 
               show_hmaps=TRUE,
               hyp_plot_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"),
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("hyp.2.rmd"))
    expect_true(file.exists("hyp.2.rmd.html"))    
    
    # multihyp
    hyp_to_rmd(multihyp_obj, 
               file_path="multihyp.2.rmd", 
               show_hmaps=TRUE,
               hyp_plot_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"),
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("multihyp.2.rmd"))
    expect_true(file.exists("multihyp.2.rmd.html"))
    
     # multihyp
    hyp_to_rmd(lmultihyp_obj, 
               file_path="lmultihyp.2.rmd", 
               show_hmaps=TRUE,
               hyp_plot_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"),
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("lmultihyp.2.rmd"))
    expect_true(file.exists("lmultihyp.2.rmd.html"))
    
    # A combination
    hyp_to_rmd(list("hyp"=hyp_obj, "multihyp"=multihyp_obj), 
               file_path="combo.1.rmd", 
               show_hmaps=TRUE,
               hyp_plot_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"),
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("combo.1.rmd"))
    expect_true(file.exists("combo.1.rmd.html"))      
    
    hyp_to_rmd(list("hyp"=hyp_obj, "multihyp"=multihyp_obj), 
               file_path="combo.2.rmd", 
               show_plots=FALSE,
               show_hmaps=TRUE,
               show_tables=FALSE,
               hyp_emap_args=list(similarity_metric="jaccard_similarity"),
               hyp_hmap_args=list(val="fdr"))
    expect_true(file.exists("combo.2.rmd"))
    expect_true(file.exists("combo.2.rmd.html"))    
        
    hyp_to_rmd(list("hyp"=hyp_obj, "multihyp"=multihyp_obj), 
               file_path="combo.3.rmd", 
               show_plots=FALSE,
               show_emaps=FALSE,
               show_hmaps=TRUE,
               show_tables=FALSE,
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("combo.3.rmd"))
    expect_true(file.exists("combo.3.rmd.html"))
})
