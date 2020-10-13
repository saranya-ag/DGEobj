context("DGEobj - tests for attributes.R functions")


test_that("showAttributes prints the attributes associated with a DEGobj", {

    # capture the console output for DGEobj object and match the length of output
    output <- capture_output_lines(showAttributes(DGEobj))
    expect_gt(length(output), 1000)

    # capture the console output for NULL object
    expect_setequal(capture_output_lines(showAttributes(NULL)), c("[1] \"dataName:\"", "[1] \"atnames: \"", "[1] \"dataName:\"", "[1] \"atnames: \""))
})

test_that("setAttributes set one or more attributes on an object", {

    # create arbitrary input to set as attributes in DGEobj
    new_attributes <- list("attribute1" = runif(100, min = 0, max = 2), "attribute2" = LETTERS)
    new_dgeobj     <- setAttributes(DGEobj, new_attributes)
    output         <- getAttributes(new_dgeobj)

    # attribute type
    expect_type(output, "list")

    # does set attributes exists in DGEobj?
    expect_true(exists("attribute1", where = output))
    expect_true(exists("attribute2", where = output))
    expect_setequal(output$attribute2, LETTERS)
})

test_that("getAttributes returns the all attributes", {

    output <- getAttributes(DGEobj)
    # attribute type
    expect_type(output, "list")

    # does all the attributes exists in DGEobj?
    expect_true(exists("objDef", where = output))
    expect_true(exists("type", where = output))
    expect_true(exists("basetype", where = output))
    expect_true(exists("parent", where = output))
    expect_true(exists("funArgs", where = output))

    # does excluded attributes exists in DGEobj?
    output <- getAttributes(DGEobj, excludeList = list("type", "basetype"))
    expect_false(exists("type", where = output))
    expect_false(exists("basetype", where = output))

    # does NULL object returns NULL object?
    expect_null(getAttributes(NULL))
})

test_that("showMeta returns the attributes associated with an object", {

    # check the class of returned object
    output <- showMeta(DGEobj)
    expect_s3_class(output, "data.frame")

    # verifying the value for attribute class
    expect_equal(output$Value[output$Attribute == "class"], "DGEobj")

    # does NULL object returns NULL object?
    expect_null(showMeta(NULL))
})
