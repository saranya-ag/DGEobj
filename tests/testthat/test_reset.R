context("DGEobj - tests for reset.R functions")


test_that('reset.R: ', {

    # testing DGEobj without levels
    expect_error(resetDGEobj(DGEobj),
                 regexp = "The DGEobj must have a 'level' attribute specified.",
                 fixed  = TRUE)

    # testing DGEobj without platformType
    test_DGEobj <- setAttributes(item = DGEobj, list("level" = "gene"))
    test_DGEobj <- setAttributes(item = test_DGEobj, list("PlatformType" = NULL))
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "Required attribute \"PlatformType\" is missing.",
                 fixed  = TRUE)

    # testing DGEobj without counts_orig matrix
    test_DGEobj <- setAttributes(item = test_DGEobj, list("PlatformType" = "RNA-Seq"))
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "The requested itemName should be in the DGEobj. Use names(dgeObj) to see the available items.",
                 fixed  = TRUE)

    # testing DGEobj with unavailable data
    names(test_DGEobj) <- c("counts_orig", "counts", "design_orig", "design", "peptideAnnotation_orig")
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "Gene/isoform/exon/protein data not found",
                 fixed  = TRUE)

    # testing valid object
    for (matrix_name in  c("geneData_orig", "isoformData_orig", "exonData_orig", "proteinData_orig")) {
        names(test_DGEobj) <- c( "counts_orig" , "counts", "design_orig" , "design", matrix_name)
        # Warning  expected as current object could not build the GRanges object!
        expect_warning({test_reset_DgeObj <- resetDGEobj(test_DGEobj)},
                       regexp = "Couldn't build a GRanges object!",
                       fixed  = TRUE)

        expect_s3_class(test_reset_DgeObj, "DGEobj")
    }

    # testing DGEobj with bad platformType
    test_DGEobj1 <- setAttributes(item = test_DGEobj, list("PlatformType" = "DNA-Seq"))
    expect_error(resetDGEobj(test_DGEobj1),
                 regexp = "The PlatformType attribute value was not recognized!",
                 fixed  = TRUE)

    # testing DGEobj with effectiveLength_orig data
    test_DGEobj <- addItem(test_DGEobj,
                           item = test_DGEobj$proteinData_orig,
                           itemName = "effectiveLength_orig",
                           itemType = "effectiveLength_orig")
    attr(test_DGEobj, "objDef")$type[["effectiveLength"]] <- "meta"
    expect_warning({test_reset_DgeObj <- resetDGEobj(test_DGEobj)},
                   regexp = "Couldn't build a GRanges object!",
                   fixed  = TRUE)

    expect_s3_class(test_reset_DgeObj, "DGEobj")
})
