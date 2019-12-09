test_that("hyp_to_table() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets_obj <- testdat$gsets
    rgsets_obj <- testdat$rgsets
    
    signature <- testdat$signature
    experiment <- testdat$experiment
    
    hyp_obj <- hypeR(signature, gsets_obj)
    multihyp_obj <- hypeR(experiment, gsets_obj)
    
    # A single file
    hyp_to_table(hyp_obj, file_path="signature.txt", sep="\t")
    expect_true(file.exists("signature.txt"))
    
    # Multiple files within a directory
    hyp_to_table(multihyp_obj, file_path="experiment", sep="\t")
    expect_true(file.exists("experiment/Signature 1.txt"))
    expect_true(file.exists("experiment/Signature 2.txt"))
    expect_true(file.exists("experiment/Signature 3.txt"))
})
