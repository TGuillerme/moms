context("vdp")

## Test make
test_that("make.vdp works", {

    ## Works exactly with no extra points
    test <- make.vdp()

    ## Right output
    expect_is(
        test
        , "list")
    expect_equal(
        length(test)
        , 8)
    expect_equal(
        names(test)
        , c("base", "diff.vol", "diff.den", "diff.pos", "diff.vol_den", "diff.vol_pos", "diff.den_pos", "diff.vol_pos_den"))
    ## Exact values
    expect_equal(unique(c(test[[1]])), c(-0.5, 0.5, -0.25, 0.25))
    expect_equal(unique(c(test[[2]])), c(-1, 1, -0.5, 0.5))
    expect_equal(unique(c(test[[3]])), c(-0.5, 0.5, -0.125, 0.125))
    expect_equal(unique(c(test[[4]])), c(0, 1, 0.25, 0.75))
    expect_equal(unique(c(test[[5]])), c(-1, 1, -0.125, 0.125))
    expect_equal(unique(c(test[[6]])), c(0.25, 0.75, 0.375, 0.625))
    expect_equal(unique(c(test[[7]])), c(0, 1, 0.375, 0.625))
    expect_equal(unique(c(test[[8]])), c(0.450, 0.550, 0.375, 0.625))

    ## Works with extra points
    test <- make.vdp(extra.points = 10)
    for(i in 1:length(test)) {
        expect_equal(dim(test[[i]]), c(2, 28))
    }

    ## Works different base range
    test <- make.vdp(base.range = c(-10, 10))
    expect_equal(unique(c(test[[1]])), c(-10, 10, -5, 5))
})

## Test dispRity
test_that("plot.vdp works", {
    expect_error(dispRity.vdp(make.vdp(), volume = "c(1, 32)",
                                     density = c(mean, neighbours),
                                     position = c(mean, displacements)))
    ## Get some disparity values
    test <- dispRity.vdp(make.vdp(), volume = c(prod, ranges),
                                     density = c(mean, neighbours),
                                     position = c(mean, displacements),
                                     base.relative = FALSE)
    
    expect_equal(length(test), 3)
    expect_equal(names(test), c("volume", "density", "position"))
    expect_equal(
        unname(unlist(test[[1]]))
        ,c(1, 4, 1, 1, 4, 0.25, 1, 0.062)
        )
    expect_equal(
        unname(unlist(test[[2]]))
        ,c(0.354, 0.707, 0.390, 0.354, 0.744, 0.177, 0.390, 0.103)
        )
    expect_equal(
        unname(unlist(test[[3]]))
        ,c(1, 1, 1, 1.663, 1, 3.090, 2.634, 7.043)
        )

    ## Get some disparity values
    test <- dispRity.vdp(make.vdp(), volume = c(prod, ranges),
                                     density = c(mean, neighbours),
                                     position = c(mean, displacements),
                                     base.relative = TRUE)
    expect_equal(
        unname(unlist(test[[1]]))
        ,c(1, 4, 1, 1, 4, 0.25, 1, 0.062)
        )
    expect_equal(
        unname(unlist(test[[2]]))
        ,c(0.354, 0.707, 0.390, 0.354, 0.744, 0.177, 0.390, 0.103)/0.354
        )
    expect_equal(
        unname(unlist(test[[3]]))
        ,c(1, 1, 1, 1.663, 1, 3.090, 2.634, 7.043)
        )
})

## Test plot
test_that("plot.vdp works", {
    test <- make.vdp()
    expect_null(plot.vdp(test))
    expect_null(plot.vdp(test, limits = c(-100, 100)))
    expect_null(plot.vdp(test, pch = 1))
    expect_null(plot.vdp(test, col = "red"))

    ## Add disparity values
    disp <- dispRity.vdp(test, volume = c(prod, ranges),
                               density = c(mean, neighbours),
                               position = c(mean, displacements),
                               base.relative = TRUE)

    expect_null(plot.vdp(test, disparity = disp))

})


