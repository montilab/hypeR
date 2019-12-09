test_that("gsets object is working", {
    
    genesets <- list("GSET1" = c("GENE1", "GENE2", "GENE3"),
                     "GSET2" = c("GENE4", "GENE5", "GENE6"),
                     "GSET3" = c("GENE7", "GENE8", "GENE9"))

    # Basic functionality
    gsets_obj <- gsets$new(genesets=genesets, name="example", version="1.0")
    expect_is(gsets_obj, "gsets")
    expect_is(gsets_obj, "R6")   
    expect_output(print(gsets_obj))
    expect_equal(gsets_obj$genesets, genesets)
    expect_equal(gsets_obj$name, "example")
    expect_equal(gsets_obj$version, "1.0")
    
    # Reduce genesets on a background distribution of symbols
    background <- c("GENE1", "GENE2", "GENE5", "GENE7", "GENE8", "GENE9")
    gsets_obj_reduce <- gsets_obj$reduce(background)
    expect_output(print(gsets_obj))
    expect_equal(gsets_obj_reduce$genesets, list("GSET1" = c("GENE1", "GENE2"),
                                                 "GSET2" = c("GENE5"),
                                                 "GSET3" = c("GENE7", "GENE8", "GENE9")))    
    
    # Warnings and errors
    expect_warning(gsets$new(genesets=genesets))
    expect_warning(gsets$new(genesets=genesets, name="example"))
    expect_warning(gsets$new(genesets=genesets, version="1.0"))
    expect_error(gsets$new(c("GENE1", "GENE2", "GENE3")))
    expect_error(gsets$new(list(c("GENE1", "GENE2", "GENE3"))))
})
