context("vdp")

## Test make
test_that("vdp.make works", {

    ## Works exactly with no extra points
    test <- vdp.make()

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
    expect_equal(unique(c(test[[2]])), c(-1, 1, -0.75, 0.75))
    expect_equal(unique(c(test[[3]])), c(-0.5, 0.5, -0.17675, 0.17675))
    expect_equal(unique(c(test[[4]])), c(0, 1, 0.25, 0.75))
    expect_equal(unique(c(test[[5]])), c(-1, 1, -0.125, 0.125))
    expect_equal(unique(c(test[[6]])), c(-0.25, 1.25, 0, 1))
    expect_equal(unique(c(test[[7]])), c(0, 1, 0.32325, 0.67675))
    expect_equal(unique(c(test[[8]])), c(0.450, 0.550, 0.375, 0.625))

    ## Works with extra points
    test <- vdp.make(extra.points = 10)
    for(i in 1:length(test)) {
        expect_equal(dim(test[[i]]), c(2, 28))
    }

    ## Works different base range
    test <- vdp.make(base.range = c(-10, 10))
    expect_equal(unique(c(test[[1]])), c(-10, 10, -5, 5))
})

## Test dispRity
test_that("vdp.dispRity works", {
    expect_error(vdp.dispRity(vdp.make(), volume = "c(1, 32)",
                                     density = c(mean, neighbours),
                                     position = c(mean, displacements)))
    ## Get some disparity values
    test <- vdp.dispRity(vdp.make(), volume = c(prod, ranges),
                                     density = c(mean, neighbours),
                                     position = c(mean, displacements),
                                     base.relative = FALSE)
    
    expect_equal(length(test), 3)
    expect_equal(names(test), c("volume", "density", "position"))
    expect_equal(
        unname(unlist(test[[1]]))
        ,c(1, 4, 1, 1, 4, 2.25, 1, 0.062)
        )
    expect_equal(
        unname(unlist(test[[2]]))
        ,c(0.354, 0.354, 0.405, 0.354, 0.744, 0.354, 0.405, 0.103)
        )
    expect_equal(
        unname(unlist(test[[3]]))
        ,c(1, 1, 1, 1.663, 1, 1.154, 2.061, 7.043)
        )

    ## Get some disparity values
    test <- vdp.dispRity(vdp.make(), volume = c(prod, ranges),
                                     density = c(mean, neighbours),
                                     position = c(mean, displacements),
                                     base.relative = TRUE)
    expect_equal(
        unname(unlist(test[[1]]))
        ,c(1, 4, 1, 1, 4, 2.25, 1, 0.062)
        )
    expect_equal(
        unname(unlist(test[[2]]))
        ,c(0.354, 0.354, 0.405, 0.354, 0.744, 0.354, 0.405, 0.103)/0.354
        )
    expect_equal(
        unname(unlist(test[[3]]))
        ,c(1, 1, 1, 1.663, 1, 1.154, 2.061, 7.043)
        )
})

## Test table
test_that("vdp.plot works", {
    test <- vdp.make()
    ## Add disparity values
    disp <- vdp.dispRity(test, volume = c(prod, ranges),
                               density = c(mean, neighbours),
                               position = c(mean, displacements),
                               base.relative = TRUE)

    test_table <- vdp.check.table(disp, test)
    expect_equal(unname(test_table), matrix(round(unlist(disp), 3), nrow = 3, byrow = TRUE))
})


## Test plot
test_that("vdp.plot works", {
    test <- vdp.make()
    expect_null(vdp.plot(test))
    expect_null(vdp.plot(test, limits = c(-100, 100)))
    expect_null(vdp.plot(test, pch = 1))
    expect_null(vdp.plot(test, col = "red"))

    ## Add disparity values
    disp <- vdp.dispRity(test, volume = c(prod, ranges),
                               density = c(mean, neighbours),
                               position = c(mean, displacements),
                               base.relative = TRUE)

    expect_null(vdp.plot(test, disparity = disp))

})


