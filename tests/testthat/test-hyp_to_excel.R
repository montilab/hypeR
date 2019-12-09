test_that("hyp_to_excel() is working", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets_obj <- testdat$gsets
    rgsets_obj <- testdat$rgsets
    
    signature <- testdat$signature
    experiment <- testdat$experiment
    
    hyp_obj <- hypeR(signature, gsets_obj)
    multihyp_obj <- hypeR(experiment, rgsets_obj)
    
    # A single excel file with one tab
    hyp_to_excel(hyp_obj, file_path="hyp.xlsx")
    expect_true(file.exists("hyp.xlsx"))
    
    # A single excel file with multiple tabs
    hyp_to_excel(multihyp_obj, file_path="multihyp.xlsx")
    expect_true(file.exists("multihyp.xlsx"))
})
