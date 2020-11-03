context("DGEobj - tests for utils.R functions")


test_that('utils.R: dim()/dimnames()', {
    dim_DGEobj <- dim(DGEobj)

    expect_equal(dim_DGEobj[1], 5900)
    expect_equal(dim_DGEobj[2], 165)

    dimnames_DGEobj <- dimnames(DGEobj)

    expect_type(dimnames_DGEobj, 'list')
    expect_equal(length(dimnames_DGEobj), 2)
    expect_setequal(names(dimnames_DGEobj), c("rownames", "colnames"))
    expect_equal(length(dimnames_DGEobj[[1]]), 5900)
    expect_equal(length(dimnames_DGEobj[[2]]), 165)
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
})

test_that('utils.R: print()', {
    expect_output(print(DGEobj), "ItemName")
})

test_that('utils.R: incorrect usage', {

})

