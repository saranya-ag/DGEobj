require(testthat)
require(stringr)

require(DGEobj)
require(GenomicRanges)

DGEobj <- readRDS(system.file("testdata", "DGEobj1.RDS", package = "DGEobj", mustWork = TRUE))
g_dim <- c(32883, 48)
