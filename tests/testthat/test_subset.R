context("DGEobj - tests for subset functions")


test_that('subset.R: subset()', {
    subsetDGEobj_1.0 <- subset(DGEobj, row = c(1:5))
    subsetDGEobj_1.1 <- DGEobj[c(1:5), ]
    subsetDGEobj_1.2 <- DGEobj[row = c(1:5) ]

    expect_equal(subsetDGEobj_1.0, subsetDGEobj_1.1)
    expect_equal(subsetDGEobj_1.0, subsetDGEobj_1.2)
    expect_equal(dim(subsetDGEobj_1.0), c(5, g_dim[2]))

    subsetDGEobj_2.0 <- subset(DGEobj, col = c(1:5))
    subsetDGEobj_2.1 <- DGEobj[, c(1:5)]
    subsetDGEobj_2.2 <- DGEobj[col = c(1:5)]

    expect_equal(subsetDGEobj_2.0, subsetDGEobj_2.1)
    expect_equal(subsetDGEobj_2.0, subsetDGEobj_2.2)
    expect_equal(dim(subsetDGEobj_2.0), c(g_dim[1], 5))

    subsetDGEobj_3.0 <- subset(DGEobj, row = c(1:5), col = c(1:5))
    subsetDGEobj_3.1 <- DGEobj[c(1:5), c(1:5)]
    subsetDGEobj_3.2 <- DGEobj[row = c(1:5), col = c(1:5)]

    expect_equal(subsetDGEobj_3.0, subsetDGEobj_3.1)
    expect_equal(subsetDGEobj_3.0, subsetDGEobj_3.2)
    expect_equal(dim(subsetDGEobj_3.0), c(5, 5))

    subsetDGEobj_4 <- subset(DGEobj,
                             row = c("ENSRNOG00000046319", "ENSRNOG00000048783"),
                             col = c("T_20170823MAN1_C05P01", "T_20170823MAN1_H06P01"))
    expect_equal(dim(subsetDGEobj_4), c(2, 2))

    expect_equal(DGEobj, DGEobj[])
    expect_equal(DGEobj, subset(DGEobj))

    debug_messages <- capture_output(subset(DGEobj, row = c(1:5), debug = TRUE))
    expect_match(debug_messages,
                 regexp = paste0("subsetting counts_orig meta .* dim: ", g_dim[1], ":", g_dim[2], ".*"))
})

test_that('subset.R: incorrect usage', {
    expect_error(subset(DGEobj, row = c(50000:50005)),
                 regexp = "row coordinates out of range")
    expect_error(DGEobj[c(50000:50005)],
                 regexp = "row coordinates out of range")
    expect_error(subset(DGEobj, col = c(50000:50005)),
                 regexp = "col coordinates out of range")
    expect_error(DGEobj[, c(50000:50005)],
                 regexp = "col coordinates out of range")
    expect_warning(subset(DGEobj, row = LETTERS),
                   regexp = "26 items in row index not found in rownames(x)",
                   fixed = TRUE)
    expect_warning(DGEobj[LETTERS],
                   regexp = "26 items in row index not found in rownames(x)",
                   fixed = TRUE)
    expect_warning(subset(DGEobj, col = LETTERS),
                   regexp = "26 items in col index not found in colnames(x)",
                   fixed = TRUE)
    expect_warning(DGEobj[, LETTERS],
                   regexp = "26 items in col index not found in colnames(x)",
                   fixed = TRUE)
})
