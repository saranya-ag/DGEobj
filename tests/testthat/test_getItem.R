context("DGEobj - tests for getItem.R functions")


test_that('getItem function works successfully on a DGE obj', {

    # get design object
    getItem_DGEobj_test <- DGEobj::getItem(DGEobj, 'design')

    expect_true(is.data.frame(getItem_DGEobj_test))
    expect_equal(nrow(getItem_DGEobj_test), 165)
    expect_equal(ncol(getItem_DGEobj_test), 13)

    # should get an error when you try to access something that doesn't exist
    expect_error(DGEobj::getItem(DGEobj, 'counts'))
})

test_that('getItems function works succesfully on a DGE obj', {

    # get one item
    getItems_DGEobj_one_test <- DGEobj::getItems(DGEobj, 'intensity')

    expect_true(is.matrix(getItems_DGEobj_one_test))
    expect_equal(nrow(getItems_DGEobj_one_test), 5900) &&
        expect_equal(ncol(getItems_DGEobj_one_test), 165)

    # get multiple items
    getItems_DGEobj_two_test <- DGEobj::getItems(DGEobj, c('intensity', 'design'))

    expect_type(getItems_DGEobj_two_test, 'list')
    expect_equal(length(getItems_DGEobj_two_test), 2)
    expect_setequal(names(getItems_DGEobj_two_test), c("intensity", "design"))

    # warning when ask for one / two things that doesn't exist
    expect_warning(DGEobj::getItems(DGEobj, c('intensity', 'counts')))

    # warning when both don't exist
    expect_warning(DGEobj::getItems(DGEobj, c('levels', 'counts')))
})

test_that('getType retrieves data for items of specified types from a DGEobj', {

    # get type that exists
    getType_DGEobj_test <- DGEobj::getType(DGEobj, "design")

    expect_type(getType_DGEobj_test, 'list')
    expect_equal(length(getType_DGEobj_test), 1)
    expect_setequal(names(getType_DGEobj_test), c("design"))

    # test with parent arg (TBD when we have different DGEobj to work with)
    # TBD

    # get type that doesn't exist
    expect_warning(DGEobj::getType(DGEobj, "counts"))
})

test_that('getBaseType succesfully retrieves data from a DGEobj of the specified base type', {

    # get one baseType
    getBaseType_DGEobj_test <- DGEobj::getBaseType(DGEobj, "col")

    expect_type(getBaseType_DGEobj_test, 'list')
    expect_equal(length(getBaseType_DGEobj_test), 1)
    expect_setequal(names(getBaseType_DGEobj_test), c("design"))

    # get two baseTypes
    # getBaseType_DGEobj_test_two <- DGEobj::getBaseType(DGEobj, c("col", "meta"))

    # ask for valid basetype that isn't found
    expect_warning(DGEobj::getBaseType(DGEobj, "row"))

    # ask for a baseType that isn't allowed
    expect_error(DGEobj::getBaseType(DGEobj, "counts"))
})

test_that('baseType succesfully reveals the base types of a DGEobj', {

    # check for baseType of a few item types that do exist in the DGEobj
    expect_equal(DGEobj::baseType(DGEobj, "intensity"), "assay")
    expect_equal(DGEobj::baseType(DGEobj, "design"), "col")
    expect_equal(DGEobj::baseType(DGEobj, "intensity_orig"), "meta")

    # check for baseType of an item type that isn't in our DGEobj but could be
    expect_equivalent(DGEobj::baseType(DGEobj, "counts"), "assay")

    # check for baseType of an item type that isn't real
    expect_error(DGEobj::baseType(DGEobj, "dog"))
})

test_that('baseTypes returns a list of base types and also the base types of a DGE obj', {

    # return the general base types
    expect_setequal(baseTypes(), c("row", "col", "assay", "meta" ))

    # return the base types of our DGEobj
    expect_setequal(baseTypes(DGEobj), c("row", "col", "assay", "meta"))
})
