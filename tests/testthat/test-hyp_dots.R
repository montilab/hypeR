hyp_dots_tests <- function(hyp_obj, return_obj=FALSE) {
    expect_silent(hyp_dots(hyp_obj, top=NULL))
    expect_silent(hyp_dots(hyp_obj))
    expect_silent(hyp_dots(hyp_obj, val="pval"))
    expect_silent(hyp_dots(hyp_obj, val="fdr"))
    expect_silent(hyp_dots(hyp_obj, abrv=20))
    expect_silent(hyp_dots(hyp_obj, title="title"))
    expect_silent(hyp_dots(hyp_obj, size_by="genesets"))
    expect_silent(hyp_dots(hyp_obj, size_by="significance"))
    expect_silent(hyp_dots(hyp_obj, size_by="none"))
    p <- hyp_dots(hyp_obj)
    expect_is(p, "gg")
    if (return_obj) return(hyp_obj)
}

test_that("hyp_dots() is working", {

    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    gsets_obj <- testdat$gsets
    rgsets_obj <- testdat$rgsets

    # Overrepresentation (signature)
    signature <- testdat$signature
    experiment <- testdat$experiment

    hypeR(signature, gsets_obj, test="hypergeometric", background=100) %>%
    hyp_dots_tests()
    hypeR(signature, rgsets_obj, test="hypergeometric", background=100) %>%
    hyp_dots_tests()
    p <- hypeR(experiment, gsets_obj, test="hypergeometric", background=100) %>%
         hyp_dots()
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "gg")

    # Enrichment (ranked signature)
    signature <- names(testdat$weighted_signature)
    experiment <- lapply(testdat$weighted_experiment, names)

    hypeR(signature, gsets_obj, test="kstest") %>%
    hyp_dots_tests()
    hypeR(signature, rgsets_obj, test="kstest") %>%
    hyp_dots_tests()
    p <- hypeR(experiment, gsets_obj, test="kstest") %>%
         hyp_dots()
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "gg")

    # Enrichment (weighted signature)
    signature <- testdat$weighted_signature
    experiment <- testdat$weighted_experiment

    hypeR(signature, gsets_obj, test="kstest") %>%
    hyp_dots_tests()
    hypeR(signature, rgsets_obj, test="kstest") %>%
    hyp_dots_tests()
    p <- hypeR(experiment, gsets_obj, test="kstest") %>%
         hyp_dots()
    expect_equal(length(p), 3)
    expect_equal(names(p), c("Signature 1", "Signature 2", "Signature 3"))
    expect_is(p[["Signature 3"]], "gg")
})

test_that("hyp_dots() thresholds zero p-values and FDR before plotting", {

    hyp_df <- data.frame(
        label = c("pathway_a", "pathway_b"),
        pval = c(0, 0.01),
        fdr = c(0, 0.02),
        overlap = c(3, 4),
        geneset = c(10, 20)
    )
    hyp_obj <- hyp$new(hyp_df)

    p <- hyp_dots(hyp_obj, val="pval", size_by="significance", top=NULL)
    expect_true(all(p$data$significance >= .Machine$double.eps))
    expect_true(all(p$data$size >= .Machine$double.eps))

    p <- hyp_dots(hyp_obj, val="fdr", top=NULL)
    expect_true(all(p$data$significance >= .Machine$double.eps))

    multihyp_obj <- multihyp$new(list("Signature 1"=hyp_obj, "Signature 2"=hyp_obj))
    p <- hyp_dots(multihyp_obj, val="pval", size_by="significance", top=NULL, merge=TRUE)
    expect_true(all(p$data$significance >= .Machine$double.eps))
    expect_true(all(p$data$size >= .Machine$double.eps))
})
