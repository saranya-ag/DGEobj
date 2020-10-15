context("DGEobj - tests for subset functions")


test_that('subset.R: subset()', {
    subsetDGEobj_1.0 <- subset(DGEobj, row = c(1:5))
    subsetDGEobj_1.1 <- DGEobj[c(1:5), ]

    expect_equal(subsetDGEobj_1.0, subsetDGEobj_1.1)
    expect_equal(dim(subsetDGEobj_1.0), c(5, 165))

    subsetDGEobj_2.0 <- subset(DGEobj, col = c(1:5))
    subsetDGEobj_2.1 <- DGEobj[, c(1:5)]

    expect_equal(subsetDGEobj_2.0, subsetDGEobj_2.1)
    expect_equal(dim(subsetDGEobj_2.0), c(5900, 5))

    subsetDGEobj_3.0 <- subset(DGEobj, row = c(1:5), col = c(1:5))
    subsetDGEobj_3.1 <- DGEobj[c(1:5), c(1:5)]

    expect_equal(subsetDGEobj_3.0, subsetDGEobj_3.1)
    expect_equal(dim(subsetDGEobj_3.0), c(5, 5))
})

test_that('subset.R: incorrect usage', {
    expect_error(subset(DGEobj, row = c(50000:50005)))
    expect_error(DGEobj[c(50000:50005)])
    expect_error(subset(DGEobj, col = c(50000:50005)))
    expect_error(DGEobj[, c(50000:50005)])
})
