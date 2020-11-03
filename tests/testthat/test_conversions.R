context("DGEobj - tests for conversions.R functions")

.get_rse <- function() {
    vals <- matrix(runif(20 * 6, 1, 1e4), 20)
    rowRanges <- GRanges(rep(c("chr1", "chr2"), c(5, 15)),
                         IRanges(sample(1000L, 20), width = 100),
                         strand = Rle(c("+", "-"), c(12, 8)),
                         seqlengths = c(chr1 = 1800, chr2 = 1300))

    colData <- DataFrame(Treatment = rep(c("ChIP", "Input"), 3),
                         row.names = LETTERS[1:6])

    rse <- SummarizedExperiment(assays = SimpleList(exprs = vals, counts = vals),
                                rowRanges = rowRanges,
                                colData = colData)

    rse
}


test_that('conversions.R: as.list()', {
    list_DGEobj <- as.list(DGEobj)
    expect_true(is.list(list_DGEobj))

    # list_DGEobj <- convertDGEobj(DGEobj, 'list')
    # requires levels on the object
})

test_that('conversions.R: convertDGEobj() to RangedSummarizedExperiment, RSE', {
    # rse_DGEobj <- convertDGEobj(DGEobj, 'RangedSummarizedExperiment')
    # requires levels on the object
})

test_that('conversions.R: convertDGEobj() to ExpressionSet, ES', {
    # rse_DGEobj <- convertDGEobj(DGEobj, 'ExpressionSet')
    # requires levels on the object
})

test_that('conversions.R: convertRSE() to ExpressionSet, ES', {
    rse <- .get_rse()

    es <- convertRSE(rse, Class = "ES")
    expect_s4_class(es, 'ExpressionSet')
})

test_that('conversions.R: convertRSE() to DGEobj', {
    rse <- .get_rse()

    # rsa_DGEobj <- convertRSE(rse, Class = "DGEobj")
    # expect_s3_class(es, 'DGEobj')
    # requirements not yet met in RSE object, need to setup more fully
})

test_that('conversions.R: incorrect usage', {
    expect_error(convertDGEobj(DGEobj, 'Bob'),
                 regexp = "The specified class must be one of \"RangedSummarizedExperiment\", \"RSE\", \"ExpressionSet\", or \"ES\".",
                 fixed  = TRUE)
    expect_error(convertDGEobj(NULL, "ES"),
                 regexp = "argument is of length zero")
    expect_error(convertDGEobj(list(), "list"),
                 regexp = "argument is of length zero")

    rse <- .get_rse()

    expect_error(convertRSE(rse, Class = "Bob"),
                 regexp = "The destination class must be one of \"DGEobj\", \"ES\", or \"ExpressionSet\".",
                 fixed  = TRUE)
    expect_error(convertRSE(rse),
                 regexp = "Specify both a RSE and a desired Class to convert the RSE to.",
                 fixed  = TRUE)
    expect_error(convertRSE(NULL, Class = "ES"),
                 regexp = "The RSE must be of class 'RangedSummarizedExperiment'.",
                 fixed  = TRUE)
})
