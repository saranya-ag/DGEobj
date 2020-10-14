context("DGEobj - tests for annotate.R functions")


test_that('annotate.R: annotateDGEobj()', {
    ann.file <- tempfile("annotations_test", fileext = ".txt")
    writeLines(c("key1='value 1'", "key2=value 2"), con = ann.file)

    ann_DGEobj <- annotateDGEobj(DGEobj, ann.file)
    expect_equal(getAttribute(ann_DGEobj, 'key1'), "'value 1'")
    expect_equal(getAttribute(ann_DGEobj, 'key2'), 'value 2')
    expect_null(getAttribute(ann_DGEobj, 'key3'))
})

test_that('annotate.R: incorrect usage', {
    expect_error(annotateDGEobj(DGEobj))
    expect_error(annotateDGEobj(DGEobj, NULL))
    expect_error(annotateDGEobj(DGEobj, "nonexistantfile.txt"))
})
