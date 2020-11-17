context("DGEobj - tests for conversions.R functions")


test_that('conversions.R: as.list()', {
    list_DGEobj <- as.list(DGEobj)
    expect_true(is.list(list_DGEobj))
})
