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
               hyp_dots_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"))
    expect_true(file.exists("hyp.1.rmd"))
    expect_true(file.exists("hyp.1.rmd.html"))    
    
    # multihyp
    hyp_to_rmd(multihyp_obj, 
               file_path="multihyp.1.rmd", 
               hyp_dots_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"))
    expect_true(file.exists("multihyp.1.rmd"))
    expect_true(file.exists("multihyp.1.rmd.html"))
    
    # multihyp
    hyp_to_rmd(lmultihyp_obj, 
               file_path="lmultihyp.1.rmd", 
               hyp_dots_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"))
    expect_true(file.exists("lmultihyp.1.rmd"))
    expect_true(file.exists("lmultihyp.1.rmd.html"))  
    
    # hyp
    expect_error(hyp_to_rmd(hyp_obj,
                            file_path="hyp.1.rmd", 
                            show_hmaps=TRUE))

    # Without relational gsets
    rgsets_obj <- testdat$rgsets
    hyp_obj <- hypeR(signature, rgsets_obj, is_rgsets=TRUE, bg=100)
    multihyp_obj <- hypeR(experiment, rgsets_obj, is_rgsets=TRUE, bg=100)
    lmultihyp_obj <- lapply(project, function(x) {hypeR(x, rgsets_obj, is_rgsets=TRUE, bg=100)})
    
    # hyp
    hyp_to_rmd(hyp_obj,
               file_path="hyp.2.rmd", 
               show_hmaps=TRUE,
               hyp_dots_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"),
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("hyp.2.rmd"))
    expect_true(file.exists("hyp.2.rmd.html"))    
    
    # multihyp
    hyp_to_rmd(multihyp_obj, 
               file_path="multihyp.2.rmd", 
               show_hmaps=TRUE,
               hyp_dots_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"),
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("multihyp.2.rmd"))
    expect_true(file.exists("multihyp.2.rmd.html"))
    
     # multihyp
    hyp_to_rmd(lmultihyp_obj, 
               file_path="lmultihyp.2.rmd", 
               show_hmaps=TRUE,
               hyp_dots_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"),
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("lmultihyp.2.rmd"))
    expect_true(file.exists("lmultihyp.2.rmd.html"))
    
    # A combination
    hyp_to_rmd(list("hyp"=hyp_obj, "multihyp"=multihyp_obj), 
               file_path="combo.1.rmd", 
               show_hmaps=TRUE,
               hyp_dots_args=list(top=10, val="pval"), 
               hyp_emap_args=list(top=10, val="pval"),
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("combo.1.rmd"))
    expect_true(file.exists("combo.1.rmd.html"))      
    
    hyp_to_rmd(list("hyp"=hyp_obj, "multihyp"=multihyp_obj), 
               file_path="combo.2.rmd", 
               show_dots=FALSE,
               show_hmaps=TRUE,
               show_tables=FALSE,
               hyp_emap_args=list(similarity_metric="jaccard_similarity"),
               hyp_hmap_args=list(val="fdr"))
    expect_true(file.exists("combo.2.rmd"))
    expect_true(file.exists("combo.2.rmd.html"))    
        
    hyp_to_rmd(list("hyp"=hyp_obj, "multihyp"=multihyp_obj), 
               file_path="combo.3.rmd", 
               show_dots=FALSE,
               show_emaps=FALSE,
               show_hmaps=TRUE,
               show_tables=FALSE,
               hyp_hmap_args=list(top=10, val="pval"))
    expect_true(file.exists("combo.3.rmd"))
    expect_true(file.exists("combo.3.rmd.html"))
})
