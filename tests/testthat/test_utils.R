context("DGEobj - tests for utils.R functions")


test_that('utils.R: dim()/dimnames()', {
    dim_DGEobj <- dim(DGEobj)

    expect_equal(dim_DGEobj[1], 5900)
    expect_equal(dim_DGEobj[2], 165)

    # dim incorrect
    expect_error(dim(DGeobj),
                 regexp = "object 'DGeobj' not found",
                 fixed  = TRUE)

    # dimnames correctly
    dimnames_DGEobj <- dimnames(DGEobj)

    expect_type(dimnames_DGEobj, 'list')
    expect_equal(length(dimnames_DGEobj), 2)
    expect_setequal(names(dimnames_DGEobj), c("rownames", "colnames"))
    expect_equal(length(dimnames_DGEobj[[1]]), 5900)
    expect_equal(length(dimnames_DGEobj[[2]]), 165)

    # dimnames incorrect
    expect_error(dimnames(DGeobj),
                 regexp = "object 'DGeobj' not found",
                 fixed  = TRUE)
})

test_that('utils.R: inventory()', {
    inventory_DGEobj <- inventory(DGEobj)

    expect_true(is.data.frame(inventory_DGEobj))
    expect_equal(nrow(inventory_DGEobj), 5)
    expect_equal(ncol(inventory_DGEobj), 8)
    expect_setequal(names(inventory_DGEobj), c("ItemName", "ItemType", "BaseType", "Parent", "Class", "Row", "Col", "DateCreated"))

    inventory_DGEobj_verbose <- inventory(DGEobj, verbose = TRUE)

    expect_true(is.data.frame(inventory_DGEobj_verbose))
    expect_equal(nrow(inventory_DGEobj_verbose), 5)
    expect_equal(ncol(inventory_DGEobj_verbose), 9)
    expect_setequal(names(inventory_DGEobj), c("ItemName", "ItemType", "BaseType", "Parent", "Class", "Row", "Col", "DateCreated"))

    # let's break it (object that doesn't exist)
    expect_error(DGEobj::inventory(DGeobj),
                 regexp = "object 'DGeobj' not found",
                 fixed  = TRUE)
})

test_that('utils.R: print()', {
    expect_output(print(DGEobj), "ItemName")

    # try to print an object that doesn't exist
    expect_error(print(DGeobj),
                 regexp = "object 'DGeobj' not found",
                 fixed  = TRUE)
})

test_that('rmItem and rmItems successfully remove named items from a DGEobj', {

    # remove design
    rmItem_design_DGEobj <- DGEobj::rmItem(DGEobj, "design")
    expect_s3_class(rmItem_design_DGEobj, "DGEobj")
    expect_equal(length(rmItem_design_DGEobj), 4)

    # try to remove two at the same time, error
    expect_error(DGEobj::rmItem(DGEobj, c("design", "intensity")),
                 regexp = "length(itemName) not equal to 1",
                 fixed  = TRUE)

    # try to remove something that doesn't exist, error
    expect_error(DGEobj::rmItem(DGEobj, "counts"),
                 regexp = "counts does not exist within DGEresult.",
                 fixed  = TRUE)

    # remove design, intensity by name
    rmItems_DGEobj <- DGEobj::rmItems(DGEobj, c("design", "intensity"))
    expect_s3_class(rmItems_DGEobj, "DGEobj")
    expect_equal(length(rmItems_DGEobj), 3)

    # remove first three items by index
    rmItems_byindex_DGEobj <- DGEobj::rmItems(DGEobj, c(1, 2, 3))
    expect_s3_class(rmItems_byindex_DGEobj, "DGEobj")
    expect_equal(length(rmItems_byindex_DGEobj), 2)

    # try to remove things that don't exist
    expect_error(DGEobj::rmItems(DGEobj, c("counts", "genes")),
                 regexp = "counts does not exist within DGEresult.",
                 fixed  = TRUE)

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
    expect_error(DGEobj::showTypes(DGeobj),
                 regexp = "object 'DGeobj' not found",
                 fixed  = TRUE)

    # forget object (some functions in this package behave this way, so a reasonable error to make)
    expect_error(DGEobj::showTypes(),
                 regexp = "argument \"dgeObj\" is missing, with no default",
                 fixed  = TRUE)
})

test_that('utils.R: incorrect usage', {

})
