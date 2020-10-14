context("DGEobj - tests for get.R functions")


test_that('get.R: getItem()', {
    getItem_DGEobj_test <- DGEobj::getItem(DGEobj, 'design')

    expect_true(is.data.frame(getItem_DGEobj_test))
    expect_equal(nrow(getItem_DGEobj_test), 165)
    expect_equal(ncol(getItem_DGEobj_test), 13)

    expect_error(DGEobj::getItem(DGEobj, 'counts'))
})

test_that('get.R: getItems()', {
    getItems_DGEobj_one_test <- DGEobj::getItems(DGEobj, 'intensity')

    expect_true(is.matrix(getItems_DGEobj_one_test))
    expect_equal(nrow(getItems_DGEobj_one_test), 5900) &&
    expect_equal(ncol(getItems_DGEobj_one_test), 165)

    getItems_DGEobj_two_test <- DGEobj::getItems(DGEobj, c('intensity', 'design'))

    expect_type(getItems_DGEobj_two_test, 'list')
    expect_equal(length(getItems_DGEobj_two_test), 2)
    expect_setequal(names(getItems_DGEobj_two_test), c("intensity", "design"))

    expect_warning(DGEobj::getItems(DGEobj, c('intensity', 'counts')))
    expect_warning(DGEobj::getItems(DGEobj, c('levels', 'counts')))
})

test_that('get.R: getType()', {
    getType_DGEobj_test <- DGEobj::getType(DGEobj, "design")

    expect_type(getType_DGEobj_test, 'list')
    expect_equal(length(getType_DGEobj_test), 1)
    expect_setequal(names(getType_DGEobj_test), c("design"))

    # test with parent arg (TBD when we have different DGEobj to work with)
    # TBD

    expect_warning(DGEobj::getType(DGEobj, "counts"))
})

test_that('get.R: getBaseType()', {
    getBaseType_DGEobj_test <- DGEobj::getBaseType(DGEobj, "col")

    expect_type(getBaseType_DGEobj_test, 'list')
    expect_equal(length(getBaseType_DGEobj_test), 1)
    expect_setequal(names(getBaseType_DGEobj_test), c("design"))

    expect_warning(DGEobj::getBaseType(DGEobj, "row"))
    expect_error(DGEobj::getBaseType(DGEobj, "counts"))
})
