test_that("rgsets object is working", {
    testdat <- readRDS(file.path(system.file("extdata", package="hypeR"), "testdat.rds"))
    rgsets_obj <- rgsets$new(gsets=testdat$gsets, nodes=testdat$nodes, edges=testdat$edges)
    expect_is(rgsets_obj, "rgsets")
    expect_is(rgsets_obj, "R6")   
    expect_output(print(rgsets_obj))
    expect_equal(names(rgsets_obj$gsets)[1:3], c("Leaf Geneset 1", "Leaf Geneset 2", "Leaf Geneset 3"))
    expect_equal(colnames(rgsets_obj$nodes), c("label", "id", "length"))
    expect_equal(rgsets_obj$nodes["G12", "length"], length(unique(c(rgsets_obj$gsets[["Leaf Geneset 7"]],
                                                                    rgsets_obj$gsets[["Leaf Geneset 8"]],
                                                                    rgsets_obj$gsets[["Leaf Geneset 9"]]))))
    
    expect_equal(colnames(rgsets_obj$edges), c("from", "to"))
    expect_equal(dim(rgsets_obj$edges), c(10,2))
    rgsets_obj_subset <- rgsets_obj$subset(c("Leaf Geneset 1", "Leaf Geneset 5", "Leaf Geneset 8"))
    expect_equivalent(c("G1","G11","G12","G13","G14","G15","G5","G8"), sort(rgsets_obj_subset$nodes$id))
    expect_equal(names(rgsets_obj_subset$gsets), c("Leaf Geneset 1", "Leaf Geneset 5", "Leaf Geneset 8"))
    expect_equal(dim(rgsets_obj_subset$edges), c(6,2))
    expect_equal(dim(rgsets_obj$edges), c(10,2))
})
