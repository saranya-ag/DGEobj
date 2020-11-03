context("DGEobj - tests for types.R functions")


test_that('types.R: baseType()', {
    expect_equal(baseType(DGEobj, "intensity"), "assay")
    expect_equal(baseType(DGEobj, "design"), "col")
    expect_equal(baseType(DGEobj, "intensity_orig"), "meta")

    expect_equivalent(baseType(DGEobj, "counts"), "assay")

    expect_error(baseType(DGEobj, "dog"),
                 regexp = "subscript out of bounds",
                 fixed  = TRUE)
})

test_that('types.R: baseTypes()', {
    expect_setequal(baseTypes(), c("row", "col", "assay", "meta" ))
    expect_setequal(baseTypes(DGEobj), c("row", "col", "assay", "meta"))
})

test_that('types.R: showTypes()', {
    showTypes_DGEobj <- showTypes(DGEobj)

    expect_s3_class(showTypes_DGEobj, "knitr_kable")
    expect_equal(length(showTypes_DGEobj), 50)

    warning('function does not appear to be working')
    showTypes_notPretty_DGEobj <- showTypes(DGEobj, pretty = FALSE)
    expect_s3_class(showTypes_notPretty_DGEobj, "data.frame")
    expect_equal(nrow(showTypes_notPretty_DGEobj), 48)
    expect_equal(ncol(showTypes_notPretty_DGEobj), 2)
})

test_that('types.R: newType()', {
    newType_DGEobj <- newType(DGEobj, "MyType", "meta")
    expect_true("MyType" %in% names(attr(newType_DGEobj, "objDef")$type))
    expect_equal(attr(newType_DGEobj, "objDef")$type[["MyType"]], "meta")
    expect_false("MyType" %in% attr(newType_DGEobj, "objDef")$uniqueType)

    newType_DGEobj <- newType(DGEobj, "MyType", "assay", uniqueItem = TRUE)
    expect_true("MyType" %in% names(attr(newType_DGEobj, "objDef")$type))
    expect_equal(attr(newType_DGEobj, "objDef")$type[["MyType"]], "assay")
    expect_true("MyType" %in% attr(newType_DGEobj, "objDef")$uniqueType)
})

test_that('types.R: incorrect usage', {
    expect_error(showTypes(),
                 regexp = "argument \"dgeObj\" is missing, with no default",
                 fixed  = TRUE)
    expect_error(newType(),
                 regexp = "Specify the DGEobj, itemType, and baseType. All three are required.",
                 fixed  = TRUE)
    expect_error(newType(DGEobj),
                 regexp = "Specify the DGEobj, itemType, and baseType. All three are required.",
                 fixed  = TRUE)
    expect_error(newType(DGEobj, "MyType", "badType"),
                 regexp = "The baseType must be one of the baseTypes available in the DGEobj. Use baseTypes(DGEobj) to see which are available.",
                 fixed  = TRUE)
})
