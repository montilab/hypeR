test_that("rgsets object is working", {
    
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    genesets <- testdat$rgsets$genesets
    nodes <- testdat$rgsets$nodes
    edges <- testdat$rgsets$edges
    
    # Basic functionality
    rgsets_obj <- rgsets$new(genesets=genesets, nodes=nodes, edges=edges, name="example", version="1.0")
    expect_is(rgsets_obj, "rgsets")
    expect_is(rgsets_obj, "R6")   
    expect_output(print(rgsets_obj))
    expect_equal(names(rgsets_obj$genesets)[1:3], c("Leaf Geneset 1", "Leaf Geneset 2", "Leaf Geneset 3"))
    expect_equal(colnames(rgsets_obj$nodes), c("label", "id", "length"))
    expect_equal(rgsets_obj$nodes["G12", "length"], length(unique(c(rgsets_obj$genesets[["Leaf Geneset 7"]],
                                                                    rgsets_obj$genesets[["Leaf Geneset 8"]],
                                                                    rgsets_obj$genesets[["Leaf Geneset 9"]]))))
    
    
    # Reduce functionality
    background <- c("D", "G", "A", "B", "K", "N")
    rgsets_obj_reduce <- rgsets_obj$reduce(background)
    expect_output(print(rgsets_obj_reduce))
    expect_equal(rgsets_obj_reduce$genesets[c(1,2,4)], list("Leaf Geneset 1" = c("D", "G", "A", "B", "K", "N"),
                                                            "Leaf Geneset 2" = c("N", "G", "B"),
                                                            "Leaf Geneset 4" = c("G", "N", "B")))    
    
    # Subset functionality
    expect_equal(colnames(rgsets_obj$edges), c("from", "to"))
    expect_equal(dim(rgsets_obj$edges), c(10,2))
    rgsets_obj_subset <- rgsets_obj$subset(c("Leaf Geneset 1", "Leaf Geneset 5", "Leaf Geneset 8"))
    expect_equivalent(c("G1","G11","G12","G13","G14","G15","G5","G8"), sort(rgsets_obj_subset$nodes$id))
    expect_equal(names(rgsets_obj_subset$genesets), c("Leaf Geneset 1", "Leaf Geneset 5", "Leaf Geneset 8"))
    expect_equal(dim(rgsets_obj_subset$edges), c(6,2))
    expect_equal(dim(rgsets_obj$edges), c(10,2))
    
    # Warnings and errors
    expect_warning(rgsets$new(genesets=genesets, nodes=nodes, edges=edges))
    expect_warning(rgsets$new(genesets=genesets, nodes=nodes, edges=edges, name="example"))
    expect_warning(rgsets$new(genesets=genesets, nodes=nodes, edges=edges, version="v1.0"))
})
