test_that("ggeplot() is working", {
    n <- 100
    positions <- c(1,3,6,10,12,18,23,25,60,72)
    x_axis <- c(0,1,2,3,5,6,9,10,11,12,17,18,22,23,24,25,59,60,71,72,100)
    y_axis <- c(0,0.09,0.08,0.17,0.15,0.24,0.21,0.3,0.29,0.38,0.33,0.42,0.38,0.47,0.46,0.55,0.21,0.3,0.19,0.28,0)
    p <- ggeplot(n, positions, x_axis, y_axis, title="Geneset Name")
    expect_is(p, "gg")
})
