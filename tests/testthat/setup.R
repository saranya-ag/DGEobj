require(testthat)
require(DGEobj)
require(GenomicRanges)
require(SummarizedExperiment)

DGEobj <- readRDS(system.file("testdata", "DGEobj1.RDS", package = "DGEobj", mustWork = TRUE))
