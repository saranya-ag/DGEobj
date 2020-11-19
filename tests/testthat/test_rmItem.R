context("DGEobj - tests for rmItem.R functions")


test_that('rmItem.R: rmItem()/rmItems()', {
    rmItem_design_DGEobj <- rmItem(DGEobj, "design")
    expect_s3_class(rmItem_design_DGEobj, "DGEobj")
    expect_equal(length(rmItem_design_DGEobj), 7)

    rmItems_DGEobj <- rmItems(DGEobj, list("design", "counts"))
    expect_s3_class(rmItems_DGEobj, "DGEobj")
    expect_equal(length(rmItems_DGEobj), 6)

    rmItems_byindex_DGEobj <- rmItems(DGEobj, c(1, 2, 3))
    expect_s3_class(rmItems_byindex_DGEobj, "DGEobj")
    expect_equal(length(rmItems_byindex_DGEobj), 5)
})

test_that('rmItem.R: incorrect usage', {
    expect_error(rmItem(DGEobj, c("design", "intensity")),
                 regexp = "Specify a singular itemName as a character string.",
                 fixed  = TRUE)
    expect_error(rmItem(DGEobj, "counts123"),
                 regexp = "counts123 does not exist within DGEresult.",
                 fixed  = TRUE)
    expect_error(rmItems(DGEobj, c("counts123", "genes")),
                 regexp = "counts does not exist within DGEresult.",
                 fixed  = TRUE)
    expect_error(rmItems(DGEobj, c(70000)),
                 regexp = "A value in the numeric index is larger than the number of items in dgeObj")
})
