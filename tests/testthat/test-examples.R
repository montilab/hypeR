# Example genesets
BIOCARTA <- ex_get("C2.CP.BIOCARTA")
KEGG <- ex_get("C2.CP.KEGG")
REACTOME <- ex_get("C2.CP.REACTOME")

gsets <- c(BIOCARTA, KEGG, REACTOME)

# Example signatures
signature <- hypeR::signatures$signature
experiment <- hypeR::signatures$experiment
project <- hypeR::signatures$project

test_that("hypeR() is working with single signatures", {
    # Basic output
    hyp <- hypeR(signature, REACTOME, bg=2520)
    expect_s4_class(hyp, "hyp")
    expect_equal(dim(hyp@data), c(674, 8))
    
    # Test pval_cutoff
    hyp <- hypeR(signature, REACTOME, bg=2520, pval_cutoff=0.0001)
    expect_equal(dim(hyp@data), c(5, 8)) 
    expect_equal(hyp@data$pval, c(3.3e-15, 4.4e-12, 4.2e-10, 6.7e-09, 9.9e-05))
    expect_equal(hyp@data$fdr, c(2.2e-12, 1.5e-09, 9.5e-08, 1.1e-06, 1.3e-02))    
    
    # Test fdr_cutoff
    hyp <- hypeR(signature, REACTOME, bg=2520, fdr_cutoff=0.05)
    expect_equal(dim(hyp@data), c(5, 8)) 
    expect_equal(hyp@data$pval, c(3.3e-15, 4.4e-12, 4.2e-10, 6.7e-09, 9.9e-05))
    expect_equal(hyp@data$fdr, c(2.2e-12, 1.5e-09, 9.5e-08, 1.1e-06, 1.3e-02))
    
    # Test hyp object
    expect_equal(hyp@args$gsets, REACTOME)
    expect_equal(hyp@args$pval_cutoff, 1)
    expect_equal(hyp@args$fdr_cutoff, 0.05)
})

test_that("hypeR() is working with multiple signatures", {
    # Basic output
    multihyp <- hypeR(experiment, REACTOME, bg=2520)
    expect_s4_class(multihyp, "multihyp")
    expect_equal(names(multihyp@data), c("YAP-KO Signature", "YAP-KO Up-regulated", "YAP-KO Down-regulated"))
    
    # Extracting hyp objects
    hyp <- multihyp@data[["YAP-KO Signature"]]
    expect_s4_class(hyp, "hyp")    
    expect_equal(dim(hyp@data), c(674, 8))
    expect_equal(hyp@args$gsets, REACTOME)
    expect_equal(hyp@args$pval_cutoff, 1)
    expect_equal(hyp@args$fdr_cutoff, 1)
})

test_that("hyp_show() is working", {
    hyp <- hypeR(signature, REACTOME, bg=2520)
    multihyp <- hypeR(experiment, REACTOME, bg=2520)
    
    # Should run
    expect_silent(hyp_show(hyp))
    
    # Should fail
    expect_error(hyp_show(multihyp))
})

test_that("hyp_to_excel() is working", {
    hyp <- hypeR(signature, REACTOME, bg=2520)
    multihyp <- hypeR(experiment, REACTOME, bg=2520)
    
    # A single excel file with one tab
    hyp_to_excel(hyp, file_path="hyp-pathways.xlsx")
    expect_true(file.exists("hyp-pathways.xlsx"))
    
    # A single excel file with multiple tabs
    hyp_to_excel(multihyp, file_path="multihyp-pathways.xlsx")
    expect_true(file.exists("multihyp-pathways.xlsx"))
})

test_that("hyp_to_table() is working", {
    hyp <- hypeR(signature, REACTOME, bg=2520)
    multihyp <- hypeR(experiment, REACTOME, bg=2520)
    
    # A single file
    hyp_to_table(hyp, file_path="signature.txt", sep="\t")
    expect_true(file.exists("signature.txt"))
    
    # Multiple files within a directory
    hyp_to_table(multihyp, file_path="experiment", sep="\t")
    expect_true(file.exists("experiment/YAP-KO Signature.txt"))
    expect_true(file.exists("experiment/YAP-KO Down-regulated.txt"))
    expect_true(file.exists("experiment/YAP-KO Up-regulated.txt"))
})

test_that("hyp_plot() is working", {
    hyp <- hypeR(signature, REACTOME, bg=2520)
    multihyp <- hypeR(experiment, REACTOME, bg=2520)
    
    # Handle a hyp object
    expect_silent(hyp_plot(hyp, show_plots=T, return_plots=F))
    expect_silent(hyp_plot(hyp, val="pval", show_plots=T, return_plots=F))
    expect_silent(hyp_plot(hyp, val="fdr", show_plots=T, return_plots=F))
    p <- hyp_plot(hyp, show_plots=F, return_plots=T)
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")
    
    # Handle a multihyp object
    expect_silent(hyp_plot(multihyp, show_plots=T, return_plots=F))
    expect_silent(hyp_plot(multihyp, val="pval", show_plots=T, return_plots=F))
    expect_silent(hyp_plot(multihyp, val="fdr", show_plots=T, return_plots=F))
    p <- hyp_plot(multihyp, show_plots=F, return_plots=T)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("YAP-KO Signature", "YAP-KO Up-regulated", "YAP-KO Down-regulated"))
    expect_s3_class(p[["YAP-KO Signature"]], "plotly")
    expect_s3_class(p[["YAP-KO Signature"]], "htmlwidget")
})

test_that("hyp_emap() is working", {
    hyp <- hypeR(signature, REACTOME, bg=2520)
    multihyp <- hypeR(experiment, REACTOME, bg=2520)
    
    # Handle a hyp object
    expect_silent(hyp_emap(hyp, top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp, val="pval", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp, val="fdr", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp, similarity_metric="jaccard_similarity", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(hyp, similarity_metric="overlap_similarity",  top=30, show_plots=T, return_plots=F))
    p <- hyp_plot(hyp,  top=30, show_plots=F, return_plots=T)
    expect_s3_class(p, "plotly")
    expect_s3_class(p, "htmlwidget")
    
    # Handle a multihyp object
    expect_silent(hyp_emap(multihyp, top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp, val="pval", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp, val="fdr",  top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp, similarity_metric="jaccard_similarity", top=30, show_plots=T, return_plots=F))
    expect_silent(hyp_emap(multihyp, similarity_metric="overlap_similarity",  top=30, show_plots=T, return_plots=F))
    p <- hyp_plot(multihyp, top=30, show_plots=F, return_plots=T)
    expect_equal(length(p), 3)
    expect_equal(names(p), c("YAP-KO Signature", "YAP-KO Up-regulated", "YAP-KO Down-regulated"))
    expect_s3_class(p[["YAP-KO Signature"]], "plotly")
    expect_s3_class(p[["YAP-KO Signature"]], "htmlwidget")
})

test_that("hyp_to_rmd() is working", {
    hyp <- hypeR(signature, REACTOME, bg=2520)
    multihyp <- hypeR(experiment, REACTOME, bg=2520)
    lmultihyp <- lmultihyp <- lapply(project, hypeR, REACTOME)
    
    # A hyp object
    hyp_to_rmd(hyp, file_path="hyp.rmd")
    expect_true(file.exists("hyp.rmd"))
    expect_true(file.exists("hyp.rmd.html"))

    # A multihyp object
    hyp_to_rmd(multihyp, file_path="multihyp.rmd")
    expect_true(file.exists("multihyp.rmd"))
    expect_true(file.exists("multihyp.rmd.html"))
            
    # A list of multihyp objects
    hyp_to_rmd(lmultihyp, file_path="lmultihyp.rmd")
    expect_true(file.exists("lmultihyp.rmd"))
    expect_true(file.exists("lmultihyp.rmd.html"))   
})
