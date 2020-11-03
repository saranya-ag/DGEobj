context("DGEobj - tests for init.R functions")


test_that('init.R: initDGEobj()', {

    # collect data from test object to initialize new DGEobj
    counts     <- getItem(DGEobj, "intensity_orig")
    rowData    <- getItem(DGEobj, "peptideAnnotation_orig")
    colData    <- getItem(DGEobj, "design_orig")
    level      <- "gene" # peptide level is not available
    customAttr <- list(Genome    = "Mouse.B38",
                       GeneModel = "Ensembl.R84")
    # create data frame with different row names
    rowData1   <- rowData
    colData1   <- colData
    rownames(rowData1) <- paste0("new_", rownames(rowData))
    rownames(colData1) <- paste0("new_", rownames(colData))

    # checking mismatch rownames
    expect_error({initDGEobj(counts     = counts,
                             rowData    = rowData1,
                             colData    = colData,
                             level      = level,
                             customAttr = customAttr)},
                 regexp = "The rownames for counts must match the rownames of rowData. Similarly, the colnames of counts must match the rownames of colData.",
                 fixed  = TRUE)

    expect_error({initDGEobj(counts     = counts,
                             rowData    = rowData,
                             colData    = colData1,
                             level      = level,
                             customAttr = customAttr)},
                 regexp = "The rownames for counts must match the rownames of rowData. Similarly, the colnames of counts must match the rownames of colData.",
                 fixed  = TRUE)

    # cheking for numeric sampleIds
    counts1 <- counts
    rownames(colData1) <- as.character(1:nrow(colData1))
    colnames(counts1)  <- as.character(1:nrow(colData1))

    expect_error({initDGEobj(counts     = counts1,
                             rowData    = rowData,
                             colData    = colData1,
                             level      = level,
                             customAttr = customAttr)},
                 regexp = paste0("It looks like you have numeric sample IDs (design rownames).",
                                 "\nPlease supply a more specific sample identifier.",
                                 " \nUse allowShortSampleIDs = TRUE to explicitly override this restriction"),
                 fixed  = TRUE)

    # checking warning as GRange object is not available.
    expect_warning({test_DgeObj <- initDGEobj(counts     = counts,
                                              rowData    = rowData,
                                              colData    = colData,
                                              level      = level,
                                              customAttr = customAttr)},
                   regexp = "Couldn't build a GRanges object!",
                   fixed  = TRUE)

    # create DGEobj with GRange objet
    # --- code

    # verifying class
    expect_s3_class(test_DgeObj, "DGEobj")
    expect_type(attributes(test_DgeObj), "list")

    # checking names and dimensions
    expect_setequal(names(test_DgeObj), c("counts_orig", "counts", "design_orig", "design", "geneData_orig", "geneData" ))
    expect_equal(dim(test_DgeObj), c(5900, 165))

    # verifying missing value errors
    expect_error(initDGEobj(rowData =  rowData, colData =  colData, level =  level, customAttr = customAttr),
                 regexp = "Specify the counts, colData, rowData, and level. All are required to initialize a DGEobj.",
                 fixed  = TRUE)
    expect_error(initDGEobj(counts = counts, colData =  colData, level =  level, customAttr = customAttr),
                 regexp = "Specify the counts, colData, rowData, and level. All are required to initialize a DGEobj.",
                 fixed  = TRUE)
    expect_error(initDGEobj(counts = counts, rowData =  rowData, level =  level, customAttr = customAttr),
                 regexp = "Specify the counts, colData, rowData, and level. All are required to initialize a DGEobj.",
                 fixed  = TRUE)
    expect_error(initDGEobj(counts = counts, rowData =  rowData, colData =  colData, customAttr = customAttr),
                 regexp = "Specify the counts, colData, rowData, and level. All are required to initialize a DGEobj.",
                 fixed  = TRUE)
})
