hyp_to_rmd_tests <- function(hyp_obj, file_path, ...) {
    hyp_to_rmd(hyp_obj, file_path, ...)
    expect_true(file.exists(file_path))
    expect_true(file.exists(paste(file_path, "html", sep=".")))   
}

test_that("hyp_to_rmd() is working", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets_obj <- testdat$gsets
    rgsets_obj <- testdat$rgsets
    
    # Overrepresentation (signature)
    signature <- testdat$signature
    experiment <- testdat$experiment
    project <- testdat$project      
    
    hypeR(signature, gsets_obj, background=100) %>%
    hyp_to_rmd_tests("hyp.1.rmd")
    
    multihyp_obj <- hypeR(experiment, gsets_obj, background=100) %>%
    hyp_to_rmd_tests("multihyp.1.rmd")   
    
    lapply(project, function(x) {hypeR(x, gsets_obj, background=100)}) %>%
    hyp_to_rmd_tests("lmultihyp.1.rmd")   

    hypeR(signature, rgsets_obj, background=100) %>%
    hyp_to_rmd_tests("hyp.2.rmd", show_hmaps=TRUE)  
    
    multihyp_obj <- hypeR(experiment, rgsets_obj, background=100) %>%
    hyp_to_rmd_tests("multihyp.2.rmd", show_hmaps=TRUE)
    
    lapply(project, function(x) {hypeR(x, rgsets_obj, background=100)}) %>%
    hyp_to_rmd_tests("lmultihyp.2.rmd", show_hmaps=TRUE) 
    
    lapply(project, function(x) {hypeR(x, rgsets_obj, background=100)}) %>%
    hyp_to_rmd_tests("lmultihyp.3.rmd", 
                     show_hmaps=TRUE,
                     hyp_dots_args=list(top=5, val="pval"), 
                     hyp_emap_args=list(top=5, val="pval"),
                     hyp_hmap_args=list(top=5, val="pval"))     
    
    # Enrichment (ranked signature)
    signature <- names(testdat$weighted_signature)
    experiment <- lapply(testdat$weighted_experiment, names)

    hypeR(signature, gsets_obj, test="kstest") %>%
    hyp_to_rmd_tests("hyp.4.rmd")
    
    multihyp_obj <- hypeR(experiment, gsets_obj, test="kstest") %>%
    hyp_to_rmd_tests("multihyp.4.rmd")   

    # Combinations
    hyp_obj <- hypeR(signature, rgsets_obj, background=100)
    multihyp_obj <- hypeR(experiment, rgsets_obj, background=100)

    hyp_to_rmd_tests(list("hyp"=hyp_obj, "multihyp"=multihyp_obj), "combo.1.rmd")
    
    hyp_to_rmd_tests(list("hyp"=hyp_obj, "multihyp"=multihyp_obj), "combo.2.rmd",
                                                                   show_dots=FALSE,
                                                                   show_hmaps=TRUE,
                                                                   show_tables=FALSE,
                                                                   hyp_emap_args=list(similarity_metric="jaccard_similarity"),
                                                                   hyp_hmap_args=list(val="fdr"))

     hyp_to_rmd_tests(list("hyp"=hyp_obj, "multihyp"=multihyp_obj), "combo.3.rmd",
                                                                    show_dots=FALSE,
                                                                    show_emaps=FALSE,
                                                                    show_hmaps=TRUE,
                                                                    show_tables=FALSE,
                                                                    hyp_hmap_args=list(top=10, val="pval"))
})
