context("DGEobj - tests for types.R functions")


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

test_that('showTypes successfully shows the types defined in a DGEobj', {
    # show the types for our obj (with default pretty = TRUE)
    showTypes_DGEobj <- DGEobj::showTypes(DGEobj)
    expect_s3_class(showTypes_DGEobj, "knitr_kable")
    expect_equal(length(showTypes_DGEobj), 50)

    # show types for our obj with pretty = FALSE
    showTypes_notPretty_DGEobj <- DGEobj::showTypes(DGEobj, pretty = FALSE)
    expect_null(showTypes_notPretty_DGEobj) # investigate this behavior

    # ask for types on an object that doesn't exist
    expect_error(DGEobj::showTypes(DGeobj))

    # forget object (some functions in this package behave this way, so a reasonable error to make)
    expect_error(DGEobj::showTypes())
})
