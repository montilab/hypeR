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
               show_plots = FALSE,
               show_emaps = TRUE,
               show_hmaps = FALSE)
    
    expect_true(file.exists("hyp.1.rmd"))
    expect_true(file.exists("hyp.1.rmd.html"))    
})
