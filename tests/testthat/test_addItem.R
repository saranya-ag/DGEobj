context("DGEobj - tests for addItem.R functions")


test_that('addItem.R: addItem()', {
    #meta
    add <- addItem(DGEobj, item = 'Fred Flintstone',
                   itemName = 'Cartoon',
                   itemType = 'meta',
                   itemAttr = list('MyAttribute' = 'testObject'))
    expect_equivalent(add$Cartoon, "Fred Flintstone")
    expect_equivalent(attributes(add$Cartoon), 'testObject')

    #rows cols - matrix
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
    expect_error(addItem(DGEobj, item = data.r[-1, -1], itemName = "MismatchedSize", itemType = "row"))

    add <- addItem(DGEobj, item = data.c,
                   itemName = 'MyMatrixCol',
                   itemType = 'col')

    expect_true('MyMatrixCol' %in% names(add))
    expect_equal(dim(add$MyMatrixCol), c(dims[2], 1))
    expect_error(addItem(DGEobj, item = data.c[-1, -1], itemName = "MismatchedSize", itemType = "col"))

    #assay
    dims <- dim(DGEobj)
    assay <- matrix(runif(dims[1]*dims[2]), nrow = dims[1])
    rownames(assay) <- dimnames(DGEobj$intensity)[[1]]
    colnames(assay) <- dimnames(DGEobj$intensity)[[2]]

    add <- addItem(DGEobj, item = assay,
                   itemName = 'MyAssay',
                   itemType = 'assay')

    expect_true('MyAssay' %in% names(add))
    expect_equal(dim(add$MyAssay), c(dims[1], dims[2]))
    expect_error(addItem(DGEobj, item = assay[-1, -1], itemName = "MismatchedSize", itemType = "assay"))
})

test_that('addItem.R: addItems()', {
    dims <- dim(DGEobj)
    data.r <- matrix(rep("rowval", dims[1]), nrow = dims[1])
    rownames(data.r) <- dimnames(DGEobj$intensity)[[1]]

    data.c <- matrix(rep("colval", dims[2], nrow = dims[2]))
    rownames(data.c) <- dimnames(DGEobj$intensity)[[2]]

    assay <- matrix(runif(dims[1]*dims[2]), nrow = dims[1])
    rownames(assay) <- dimnames(DGEobj$intensity)[[1]]
    colnames(assay) <- dimnames(DGEobj$intensity)[[2]]

    add <- addItems(DGEobj,
                    itemList = list("Cartoon" = "Fred Flintstone",
                                    "Historic" = "Abe Lincoln",
                                    "MyMatrixRow" = data.r,
                                    "MyMatrixCol" = data.c,
                                    "MyAssay" = assay),
                    itemTypes = list("meta", "meta", "row", "col", "assay"))

    expect_equal(add$Cartoon, "Fred Flintstone")
    expect_equal(add$Historic, "Abe Lincoln")
    expect_true('MyMatrixCol' %in% names(add))
    expect_equal(dim(add$MyMatrixCol), c(dims[2], 1))
    expect_true('MyMatrixRow' %in% names(add))
    expect_equal(dim(add$MyMatrixRow), c(dims[1], 1))
    expect_true('MyAssay' %in% names(add))
    expect_equal(dim(add$MyAssay), c(dims[1], dims[2]))
})

test_that('addItem.R: incorrect usage', {
    expect_error(addItem(matrix(rep(0, 5), nrow = 5)))

    expect_error(addItem(NULL))
    expect_error(addItem(DGEobj))
    expect_error(addItem(DGEobj, item = 'mystring', itemName = 'teststring', itemType = 'badtype'))
    expect_error(addItem(DGEobj, item = 'mystring', itemName = 'teststring', itemType = 'row'))
    expect_error(addItem(DGEobj, item = 'mystring', itemName = 'teststring', itemType = 'col'))
    expect_error(addItem(DGEobj, item = 'mystring', itemName = 'teststring', itemType = 'assay'))
    expect_error(addItem(DGEobj, item = matrix(rep(0, 25), nrow = 5), itemName = 'testmatrix', itemType = 'row'))
    expect_error(addItem(DGEobj, item = matrix(rep(0, 25), nrow = 5), itemName = 'testmatrix', itemType = 'col'))
    expect_error(addItem(DGEobj, item = matrix(rep(0, 25), nrow = 5), itemName = 'testmatrix', itemType = 'assay'))

    expect_error(addItems(NULL))
    expect_error(addItems(DGEobj))
    expect_error(addItems(DGEobj, itemList = list('teststring' = 'mystring'), itemTypes = list('badtype')))
    expect_error(addItems(DGEobj, itemList = list('teststring' = 'mystring'), itemTypes = list('row')))
    expect_error(addItems(DGEobj, itemList = list('teststring' = 'mystring'), itemTypes = list('col')))
    expect_error(addItems(DGEobj, itemList = list('teststring' = 'mystring'), itemTypes = list('assay')))
    expect_error(addItems(DGEobj, itemList = list('testmatrix' = matrix(rep(0, 25), nrow = 5)), itemTypes = list('row')))
    expect_error(addItems(DGEobj, itemList = list('testmatrix' = matrix(rep(0, 25), nrow = 5)), itemTypes = list('col')))
    expect_error(addItems(DGEobj, itemList = list('testmatrix' = matrix(rep(0, 25), nrow = 5)), itemTypes = list('assay')))
})
