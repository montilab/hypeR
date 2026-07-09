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

test_that("hyp_to_excel() filters rows by pval and fdr", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets_obj <- testdat$gsets
    signature <- testdat$signature
    
    hyp_obj <- hypeR(signature, gsets_obj)
    pval_cutoff <- stats::median(hyp_obj$data$pval)
    fdr_cutoff <- stats::median(hyp_obj$data$fdr)
    
    expected <- hyp_obj$data[hyp_obj$data$pval <= pval_cutoff & hyp_obj$data$fdr <= fdr_cutoff, , drop=FALSE]
    
    hyp_to_excel(hyp_obj, file_path="hyp_filtered.xlsx", pval=pval_cutoff, fdr=fdr_cutoff)
    expect_true(file.exists("hyp_filtered.xlsx"))
    
    observed <- openxlsx::readWorkbook("hyp_filtered.xlsx", sheet=1)
    expect_equal(nrow(observed), nrow(expected))
    if (nrow(expected) > 0) {
        expect_equal(observed$label, expected$label)
    }
})
