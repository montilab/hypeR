test_that("hyp_to_table() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    signature <- testdat$signature
    experiment <- testdat$experiment
    gsets <- testdat$gsets    

    hyp_obj <- hypeR(signature, gsets, test="hypergeometric", bg=100)
    multihyp_obj <- hypeR(experiment, gsets, test="hypergeometric", bg=100)
    
    # A single file
    hyp_to_table(hyp_obj, file_path="signature.txt", sep="\t")
    expect_true(file.exists("signature.txt"))
    
    # Multiple files within a directory
    hyp_to_table(multihyp_obj, file_path="experiment", sep="\t")
    expect_true(file.exists("experiment/Signature 1.txt"))
    expect_true(file.exists("experiment/Signature 2.txt"))
    expect_true(file.exists("experiment/Signature 3.txt"))

    ranked_signature <- testdat$ranked_signature$ranked
    ranked_experiment <- do.call(rbind, testdat$ranked_experiment)[,'ranked']
    hyp_obj <- hypeR(ranked_signature, gsets, test="kstest", bg=100)
    multihyp_obj <- hypeR(ranked_experiment, gsets, test="kstest", bg=100)
    
    # A single file
    hyp_to_table(hyp_obj, file_path="signature.txt", sep="\t")
    expect_true(file.exists("signature.txt"))
    
    # Multiple files within a directory
    hyp_to_table(multihyp_obj, file_path="experiment", sep="\t")
    expect_true(file.exists("experiment/Signature 1.txt"))
    expect_true(file.exists("experiment/Signature 2.txt"))
    expect_true(file.exists("experiment/Signature 3.txt"))    
})
