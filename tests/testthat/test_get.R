context("DGEobj - tests for get.R functions")


test_that('get.R: getItem()', {
    getItem_DGEobj_test <- getItem(DGEobj, 'design')

    expect_true(is.data.frame(getItem_DGEobj_test))
    expect_equal(nrow(getItem_DGEobj_test), 48)
    expect_equal(ncol(getItem_DGEobj_test), 8)

    expect_error(getItem(DGEobj, 'fred'),
                 regexp = "The requested itemName should be in the DGEobj. Use names(dgeObj) to see the available items.",
                 fixed = TRUE)
})

test_that('get.R: getItems()', {
    getItems_DGEobj_one_test <- getItems(DGEobj, 'counts')

    expect_true(is.matrix(getItems_DGEobj_one_test))
    expect_equal(nrow(getItems_DGEobj_one_test), g_dim[1]) &&
    expect_equal(ncol(getItems_DGEobj_one_test), g_dim[2])

    getItems_DGEobj_two_test <- getItems(DGEobj, c('counts', 'design'))

    expect_type(getItems_DGEobj_two_test, 'list')
    expect_equal(length(getItems_DGEobj_two_test), 2)
    expect_setequal(names(getItems_DGEobj_two_test), c("counts", "design"))

    expect_warning(getItems(DGEobj, c('fred', 'counts')),
                   regexp = "These item(s) not found: [fred]",
                   fixed  = TRUE)
    expect_warning(getItems(DGEobj, c('fred', 'bob')),
                   regexp = "These item(s) not found: [fred]These item(s) not found: [bob]",
                   fixed  = TRUE)
})

test_that('get.R: getType()', {
    getType_DGEobj_test <- getType(DGEobj, "design")

    expect_type(getType_DGEobj_test, 'list')
    expect_equal(length(getType_DGEobj_test), 1)
    expect_setequal(names(getType_DGEobj_test), c("design"))

    # test with parent arg (TBD when we have different DGEobj to work with)
    # TBD

    expect_message(getType_warning <- capture_warnings(DGEobj::getType(DGEobj, "fred")),
                   regexp = "Warning: no items of specified type are found.")
    expect_equal(getType_warning, "Some types were not found")
})

test_that('get.R: getBaseType()', {
    getBaseType_DGEobj_test <- getBaseType(DGEobj, "col")

    expect_type(getBaseType_DGEobj_test, 'list')
    expect_equal(length(getBaseType_DGEobj_test), 1)
    expect_setequal(names(getBaseType_DGEobj_test), c("design"))

    expect_error(getBaseType(DGEobj, "counts"),
                 regexp = "baseType must be one of: row, col, assay, meta")
})
