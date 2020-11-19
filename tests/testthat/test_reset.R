context("DGEobj - tests for reset.R functions")


test_that('reset.R: ', {
    # testing valid DGEobj
    new <- resetDGEobj(DGEobj)
    expect_s3_class(new, "DGEobj")
    expect_equivalent(new, DGEobj)

    # testing DGEobj without platformType
    test_DGEobj <- setAttributes(item = DGEobj, list("level" = "gene"))
    test_DGEobj <- setAttributes(item = test_DGEobj, list("PlatformType" = NULL))
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "Required attribute \"PlatformType\" is missing.",
                 fixed  = TRUE)

    # testing DGEobj with unavailable data
    test_DGEobj <- setAttributes(item = DGEobj, list("PlatformType" = "RNA-Seq"))
    names(test_DGEobj) <- c("counts_orig", "counts", "design_orig", "design", "peptideAnnotation_orig")
    expect_error(resetDGEobj(test_DGEobj),
                 regexp = "Gene/isoform/exon/protein data not found",
                 fixed  = TRUE)

    # testing DGEobj with bad platformType
    test_DGEobj1 <- setAttributes(item = DGEobj, list("PlatformType" = "fred"))
    expect_error(resetDGEobj(test_DGEobj1),
                 regexp = "The PlatformType attribute value was not recognized!",
                 fixed  = TRUE)
})
