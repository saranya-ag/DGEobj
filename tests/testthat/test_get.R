context("DGEobj - tests for get.R functions")


test_that('get.R: getItem()', {
    getItem_DGEobj_test <- getItem(DGEobj, 'design')

    expect_true(is.data.frame(getItem_DGEobj_test))
    expect_equal(nrow(getItem_DGEobj_test), 165)
    expect_equal(ncol(getItem_DGEobj_test), 13)

    expect_error(getItem(DGEobj, 'counts'),
                 regexp = "The requested itemName should be in the DGEobj. Use names(dgeObj) to see the available items.",
                 fixed  = TRUE)
})

test_that('get.R: getItems()', {
    getItems_DGEobj_one_test <- getItems(DGEobj, 'intensity')

    expect_true(is.matrix(getItems_DGEobj_one_test))
    expect_equal(nrow(getItems_DGEobj_one_test), 5900) &&
    expect_equal(ncol(getItems_DGEobj_one_test), 165)

    getItems_DGEobj_two_test <- getItems(DGEobj, c('intensity', 'design'))

    expect_type(getItems_DGEobj_two_test, 'list')
    expect_equal(length(getItems_DGEobj_two_test), 2)
    expect_setequal(names(getItems_DGEobj_two_test), c("intensity", "design"))

    expect_warning(getItems(DGEobj, c('intensity', 'counts')),
                   regexp = "These item(s) not found: [counts]",
                   fixed  = TRUE)
    expect_warning(getItems(DGEobj, c('levels', 'counts')),
                   regexp = "These item(s) not found: [levels]These item(s) not found: [counts]",
                   fixed  = TRUE)
})

test_that('get.R: getType()', {
    getType_DGEobj_test <- getType(DGEobj, "design")

    expect_type(getType_DGEobj_test, 'list')
    expect_equal(length(getType_DGEobj_test), 1)
    expect_setequal(names(getType_DGEobj_test), c("design"))

    # test with parent arg (TBD when we have different DGEobj to work with)
    # TBD

    expect_message(getType_warning <- capture_warnings(DGEobj::getType(DGEobj, "counts")),
                   regexp = "Warning: no items of specified type are found.")
    expect_equal(getType_warning, "Some types were not found")
})

test_that('get.R: getBaseType()', {
    getBaseType_DGEobj_test <- getBaseType(DGEobj, "col")

    expect_type(getBaseType_DGEobj_test, 'list')
    expect_equal(length(getBaseType_DGEobj_test), 1)
    expect_setequal(names(getBaseType_DGEobj_test), c("design"))

    expect_warning(getBaseType(DGEobj, "row"),
                   regexp = "Some baseTypes were not found")
    expect_error(getBaseType(DGEobj, "counts"),
                 regexp = "baseType must be one of: row, col, assay, meta",
                 fixed  = TRUE)
})
