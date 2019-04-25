# Example genesets
gsets <- readRDS(system.file("extdata/gsets.rds", package="hypeR"))
BIOCARTA <- gsets$BIOCARTA
KEGG <- gsets$KEGG
REACTOME <- gsets$REACTOME

# Example relational genesets
rgsets.lst <- readRDS(system.file("extdata/rgsets.rds", package="hypeR"))
rgsets.obj <- rgsets.lst$REACTOME

# Example signatures
signature <- hypeR::signatures$signature
experiment <- hypeR::signatures$experiment
project <- hypeR::signatures$project

test_that("hypeR() is working with single signatures", {
    # Basic output
    hyp_obj <- hypeR(signature, REACTOME, bg=2520)
    expect_is(hyp_obj, "hyp")
    expect_equal(dim(hyp_obj$data), c(674, 8))
    
    # Test pval_cutoff
    hyp_obj <- hypeR(signature, REACTOME, bg=2520, pval_cutoff=0.0001)
    expect_equal(dim(hyp_obj$data), c(5, 8)) 
    expect_equal(hyp_obj$data$pval, c(3.3e-15, 4.4e-12, 4.2e-10, 6.7e-09, 9.9e-05))
    expect_equal(hyp_obj$data$fdr, c(2.2e-12, 1.5e-09, 9.5e-08, 1.1e-06, 1.3e-02))    
    
    # Test fdr_cutoff
    hyp_obj <- hypeR(signature, REACTOME, bg=2520, fdr_cutoff=0.05)
    expect_equal(dim(hyp_obj$data), c(5, 8)) 
    expect_equal(hyp_obj$data$pval, c(3.3e-15, 4.4e-12, 4.2e-10, 6.7e-09, 9.9e-05))
    expect_equal(hyp_obj$data$fdr, c(2.2e-12, 1.5e-09, 9.5e-08, 1.1e-06, 1.3e-02))
    
    # Test hyp object
    expect_equal(hyp_obj$args$gsets, REACTOME)
    expect_equal(hyp_obj$args$pval_cutoff, 1)
    expect_equal(hyp_obj$args$fdr_cutoff, 0.05)
    
    # Test relational gsets
    expect_error(hypeR(signature, rgsets.obj, bg=2520, fdr_cutoff=0.05))
    hyp_obj <- hypeR(signature, rgsets.obj, gsets_relational=TRUE, bg=2520, pval_cutoff=0.05)
    expect_true(hyp_obj$args$gsets_relational)
    expect_is(hyp_obj$args$gsets, "rgsets")
    expect_is(hyp_obj$args$gsets, "R6")
    expect_equal(hyp_obj$data$pval, c(4.2e-10, 1.1e-04))
    expect_equal(hyp_obj$data$fdr, c(6.6e-07, 8.7e-02))
})

test_that("hypeR() is working with multiple signatures", {
    # Basic output
    multihyp_obj <- hypeR(experiment, REACTOME, bg=2520)
    expect_is(multihyp_obj, "multihyp")
    expect_equal(names(multihyp_obj$data), c("YAP-KO Signature", "YAP-KO Up-regulated", "YAP-KO Down-regulated"))
    
    # Extracting hyp objects
    hyp_obj <- multihyp_obj$data[["YAP-KO Signature"]]
    expect_is(hyp_obj, "hyp")    
    expect_equal(dim(hyp_obj$data), c(674, 8))
    expect_equal(hyp_obj$args$gsets, REACTOME)
    expect_equal(hyp_obj$args$pval_cutoff, 1)
    expect_equal(hyp_obj$args$fdr_cutoff, 1)
    
    # Test relational gsets
    expect_error(hypeR(experiment, rgsets.obj, bg=2520, fdr_cutoff=0.05))
    multihyp_obj <- hypeR(experiment, rgsets.obj, gsets_relational=TRUE, bg=2520, pval_cutoff=0.05)
    hyp_obj <- multihyp_obj$data[["YAP-KO Signature"]]
    expect_equal(dim(hyp_obj$data), c(37, 8))
    expect_is(hyp_obj$args$gsets, "rgsets")
    expect_is(hyp_obj$args$gsets, "R6")
})

test_that("hyp_show() is working", {
    hyp_obj <- hypeR(signature, REACTOME, bg=2520)
    multihyp_obj <- hypeR(experiment, REACTOME, bg=2520)
    
    # Should run
    expect_silent(hyp_show(hyp_obj))
    
    # Should fail
    expect_error(hyp_show(multihyp_obj))
})

test_that("hyp_to_excel() is working", {
    hyp_obj <- hypeR(signature, REACTOME, bg=2520)
    multihyp_obj <- hypeR(experiment, REACTOME, bg=2520)
    
    # A single excel file with one tab
    hyp_to_excel(hyp_obj, file_path="hyp-pathways.xlsx")
    expect_true(file.exists("hyp-pathways.xlsx"))
    
    # A single excel file with multiple tabs
    hyp_to_excel(multihyp_obj, file_path="multihyp-pathways.xlsx")
    expect_true(file.exists("multihyp-pathways.xlsx"))
})

test_that("hyp_to_table() is working", {
    hyp_obj <- hypeR(signature, REACTOME, bg=2520)
    multihyp_obj <- hypeR(experiment, REACTOME, bg=2520)
    
    # A single file
    hyp_to_table(hyp_obj, file_path="signature.txt", sep="\t")
    expect_true(file.exists("signature.txt"))
    
    # Multiple files within a directory
    hyp_to_table(multihyp_obj, file_path="experiment", sep="\t")
    expect_true(file.exists("experiment/YAP-KO Signature.txt"))
    expect_true(file.exists("experiment/YAP-KO Down-regulated.txt"))
    expect_true(file.exists("experiment/YAP-KO Up-regulated.txt"))
})

test_that("hyp_plot() is working", {
    hyp_obj <- hypeR(signature, REACTOME, bg=2520)
    multihyp_obj <- hypeR(experiment, REACTOME, bg=2520)
    
    # Handle a hyp object
    expect_silent(hyp_plot(hyp_obj, show_plots=T, return_plots=F))
    expect_silent(hyp_plot(hyp_obj, val="pval", show_plots=T, return_plots=F))
    expect_silent(hyp_plot(hyp_obj, val="fdr", show_plots=T, return_plots=F))
    p <- hyp_plot(hyp_obj, show_plots=F, return_plots=T)
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")
    
    # Handle a multihyp object
    expect_silent(hyp_plot(multihyp_obj, show_plots=T, return_plots=F))
    expect_silent(hyp_plot(multihyp_obj, val="pval", show_plots=T, return_plots=F))
    expect_silent(hyp_plot(multihyp_obj, val="fdr", show_plots=T, return_plots=F))
    p <- hyp_plot(multihyp_obj, show_plots=F, return_plots=T)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("YAP-KO Signature", "YAP-KO Up-regulated", "YAP-KO Down-regulated"))
    expect_s3_class(p[["YAP-KO Signature"]], "plotly")
    expect_s3_class(p[["YAP-KO Signature"]], "htmlwidget")
})

test_that("hyp_emap() is working with gsets", {
    hyp_obj <- hypeR(signature, REACTOME, bg=2520)
    multihyp_obj <- hypeR(experiment, REACTOME, bg=2520)
    
    # Handle a hyp object
    expect_silent(hyp_emap(hyp_obj, top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp_obj, val="pval", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp_obj, val="fdr", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="jaccard_similarity", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="overlap_similarity",  top=30, show_plots=T, return_plots=F))
    p <- hyp_plot(hyp_obj,  top=30, show_plots=F, return_plots=T)
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")
    
    # Handle a multihyp object
    expect_silent(hyp_emap(multihyp_obj, top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp_obj, val="pval", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp_obj, val="fdr",  top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp_obj, similarity_metric="jaccard_similarity", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp_obj, similarity_metric="overlap_similarity",  top=30, show_plots=T, return_plots=F))
    p <- hyp_plot(multihyp_obj, top=30, show_plots=F, return_plots=T)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("YAP-KO Signature", "YAP-KO Up-regulated", "YAP-KO Down-regulated"))
    expect_s3_class(p[["YAP-KO Signature"]], "plotly")
    expect_s3_class(p[["YAP-KO Signature"]], "htmlwidget")
})

test_that("hyp_emap() is working with relational gsets", {
    hyp_obj <- hypeR(signature, rgsets.obj, gsets_relational=TRUE, bg=2520)
    multihyp_obj <- hypeR(experiment, rgsets.obj, gsets_relational=TRUE, bg=2520)
    
    # Handle a hyp object
    expect_silent(hyp_emap(hyp_obj, top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp_obj, val="pval", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp_obj, val="fdr", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="jaccard_similarity", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp_obj, similarity_metric="overlap_similarity",  top=30, show_plots=T, return_plots=F))
    p <- hyp_plot(hyp_obj,  top=30, show_plots=F, return_plots=T)
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")
    
    # Handle a multihyp object
    expect_silent(hyp_emap(multihyp_obj, top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp_obj, val="pval", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp_obj, val="fdr",  top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp_obj, similarity_metric="jaccard_similarity", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp_obj, similarity_metric="overlap_similarity",  top=30, show_plots=T, return_plots=F))
    p <- hyp_plot(multihyp_obj, top=30, show_plots=F, return_plots=T)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("YAP-KO Signature", "YAP-KO Up-regulated", "YAP-KO Down-regulated"))
    expect_s3_class(p[["YAP-KO Signature"]], "plotly")
    expect_s3_class(p[["YAP-KO Signature"]], "htmlwidget")
})

test_that("hyp_to_rmd() is working", {
    hyp_obj <- hypeR(signature, REACTOME, bg=2520)
    multihyp_obj <- hypeR(experiment, REACTOME, bg=2520)
    lmultihyp_obj <- lapply(project, hypeR, REACTOME)
    
    # A hyp object
    hyp_to_rmd(hyp_obj, file_path="hyp.rmd")
    expect_true(file.exists("hyp.rmd"))
    expect_true(file.exists("hyp.rmd.html"))

    # A multihyp object
    hyp_to_rmd(multihyp_obj, file_path="multihyp.rmd")
    expect_true(file.exists("multihyp.rmd"))
    expect_true(file.exists("multihyp.rmd.html"))
            
    # A list of multihyp objects
    hyp_to_rmd(lmultihyp_obj, file_path="lmultihyp.rmd")
    expect_true(file.exists("lmultihyp.rmd"))
    expect_true(file.exists("lmultihyp.rmd.html"))   
})
