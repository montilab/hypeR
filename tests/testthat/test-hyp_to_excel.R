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
    
    ranked_signature <- testdat$ranked_signature$ranked
    ranked_experiment <- do.call(rbind, testdat$ranked_experiment)[,'ranked']
    hyp_obj <- hypeR(ranked_signature, gsets, test="kstest", bg=100)
    multihyp_obj <- hypeR(ranked_experiment, gsets, test="kstest", bg=100)
    
    # A single excel file with one tab
    hyp_to_excel(hyp_obj, file_path="hyp-pathways.xlsx")
    expect_true(file.exists("hyp-pathways.xlsx"))
    
    # A single excel file with multiple tabs
    hyp_to_excel(multihyp_obj, file_path="multihyp-pathways.xlsx")
    expect_true(file.exists("multihyp-pathways.xlsx"))    
})
