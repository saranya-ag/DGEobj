require(testthat)
require(stringr)

require(DGEobj)
require(GenomicRanges)

t_obj <- readRDS(system.file("testdata", "DGEobj1.RDS", package = "DGEobj", mustWork = TRUE))
t_dim <- c(32883, 48)
