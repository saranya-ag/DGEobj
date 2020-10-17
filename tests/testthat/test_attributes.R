context("DGEobj - tests for attributes.R functions")


test_that("attributes.R: showAttributes()", {
    output <- capture_output_lines(showAttributes(DGEobj))

    expect_gt(length(output), 1000)
    expect_setequal(capture_output_lines(showAttributes(NULL)), c("[1] \"dataName:\"", "[1] \"atnames: \"", "[1] \"dataName:\"", "[1] \"atnames: \""))
})

test_that("attributes.R: setAttributes()/getAttributes()", {
    new_attributes <- list("attribute1" = runif(100, min = 0, max = 2), "attribute2" = LETTERS)
    new_dgeobj     <- setAttributes(DGEobj, new_attributes)

    output         <- getAttributes(new_dgeobj)

    expect_type(output, "list")
    expect_true(exists("attribute1", where = output))
    expect_true(exists("attribute2", where = output))
    expect_setequal(output$attribute2, LETTERS)
})

test_that("attributes.R: setAttribute()/getAttribute()", {
    # TBD once setAttribute is ready
    # Pair with getAttribute for that specific attribute
})

test_that("attributes.R: getAttributes() returns all", {
    output <- getAttributes(DGEobj)

    expect_type(output, "list")
    expect_true(exists("objDef", where = output))
    expect_true(exists("type", where = output))
    expect_true(exists("basetype", where = output))
    expect_true(exists("parent", where = output))
    expect_true(exists("funArgs", where = output))

    output <- getAttributes(DGEobj, excludeList = list("type", "basetype"))

    expect_false(exists("type", where = output))
    expect_false(exists("basetype", where = output))
})

test_that("attributes.R: showMeta()", {
    output <- showMeta(DGEobj)

    expect_s3_class(output, "data.frame")
    expect_equal(output$Value[output$Attribute == "class"], "DGEobj")

    expect_null(showMeta(NULL))
})

test_that("attributes.R: incorrect usage", {
    expect_error(setAttributes(DGEobj, attribs = NULL),
                 regexp = "class(attribs)[[1]] not equal to \"list\"",
                 fixed  = TRUE)
    expect_error(setAttributes(DGEobj, attribs = list()),
                 regexp = "!is.null(names(attribs)) is not TRUE",
                 fixed  = TRUE)
    expect_error(getAttribute(DGEobj, NULL),
                 regexp = "'which' must be of mode character")

    expect_null(getAttributes("fred"))
    expect_null(getAttributes(NULL))
    expect_null(getAttributes(list()))

    expect_null(getAttribute("fred", "fred"))
})
