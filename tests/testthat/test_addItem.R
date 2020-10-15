context("DGEobj - tests for addItem.R functions")


test_that('addItem.R: addItem()', {
    #meta
    add <- addItem(DGEobj, item = 'Fred Flintstone',
                   itemName = 'Cartoon',
                   itemType = 'meta')
    expect_equal(add$Cartoon, "Fred Flintstone")

    #rows cols
    dims <- dim(DGEobj)
    data.r <- matrix(rep("rowval", dims[1]), nrow = dims[1])
    rownames(data.r) <- dimnames(DGEobj$intensity)[[1]]

    data.c <- matrix(rep("colval", dims[2], nrow = dims[2]))
    rownames(data.c) <- dimnames(DGEobj$intensity)[[2]]

    add <- addItem(DGEobj, item = data.r,
                   itemName = 'MyMatrixRow',
                   itemType = 'row')

    expect_true('MyMatrixRow' %in% names(add))
    expect_equal(dim(add$MyMatrixRow), c(dims[1], 1))

    add <- addItem(DGEobj, item = data.c,
                   itemName = 'MyMatrixCol',
                   itemType = 'col')

    expect_true('MyMatrixCol' %in% names(add))
    expect_equal(dim(add$MyMatrixCol), c(dims[2], 1))
})

test_that('addItem.R: addItems()', {
    #meta
    add <- addItems(DGEobj,
                    itemList = list("Cartoon"  = "Fred Flintstone",
                                    "Historic" = "Abe Lincoln"),
                    itemTypes = list("meta", "meta"))

    expect_equal(add$Cartoon, "Fred Flintstone")
    expect_equal(add$Historic, "Abe Lincoln")

    #rows cols
    dims <- dim(DGEobj)
    data.r <- matrix(rep("rowval", dims[1]), nrow = dims[1])
    rownames(data.r) <- dimnames(DGEobj$intensity)[[1]]

    data.c <- matrix(rep("colval", dims[2], nrow = dims[2]))
    rownames(data.c) <- dimnames(DGEobj$intensity)[[2]]

    add <- addItems(DGEobj,
                    itemList = list("MyMatrixRow" = data.r, "MyMatrixCol" = data.c),
                    itemTypes = list("row", "col"))

    expect_true('MyMatrixCol' %in% names(add))
    expect_equal(dim(add$MyMatrixCol), c(dims[2], 1))
    expect_true('MyMatrixRow' %in% names(add))
    expect_equal(dim(add$MyMatrixRow), c(dims[1], 1))

})

test_that('addItem.R: incorrect usage', {
    expect_error(addItem(NULL))
    expect_error(addItem(DGEobj))
    expect_error(addItem(DGEobj, item = 'fred', itemName = 'fred', itemType = 'fred'))

    expect_error(addItems(NULL))
    expect_error(addItems(DGEobj))
    expect_error(addItems(DGEobj, itemList = list('fred' = 'fred'), itemTypes = list('fred')))
})
