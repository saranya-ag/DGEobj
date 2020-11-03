#' Function mergeDGEobj (DGEobj)
#'
#' Merge two DGEobjs together.  This will effectively column bind the counts from d2 onto
#' d1.  You can choose between an inner join and left join when combining the
#' data.  This will **NOT** try to capture the workflows from both objects.  Rather,
#' it will capture just the counts, row, and col annotations and merge those items
#' into a new, combined DGEobj.  Row (gene) annotation will be taken from the 1st
#' DGEobj.  Sample annotation must have at least one common column name to be merged.
#' Only columns present in both datasets will be kept.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param d1 The first DGEobj (Required)
#' @param d2 The second DGEobj (Required)
#' @param join The join type to use (default = "inner"). Values of "inner" and
#' "left" are supported.
#' @param orig Default = TRUE. A DGEobj is typically filtered to remove non-expressed genes. However,
#' a copy of the original (unfiltered) data is still stored.  By default,
#' this function will merge the **original unfiltered** data for these two DGEobjs. Set orig = FALSE
#' to disable this behavior and thus attempt to merge the already filtered data.
#'
#' @return A DGEobj class object with the merged data.
#'
#' @examples
#' \dontrun{
#'    MyCombinedDGEobj <- mergeDGEobj(DGEobj1, DGEobj2, join = "inner", orig = TRUE)
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
mergeDGEobj <- function(d1, d2, join = "inner", orig = TRUE) {

    assertthat::assert_that(!missing(d1),
                            !missing(d2),
                            msg = "You must specify d1 and d2 (two DGEobjs to merge).")
    assertthat::assert_that(class(d1)[[1]] == "DGEobj",
                            class(d2)[[1]] == "DGEobj",
                            msg = "Both d1 and d2 must be of class 'DGEobj'.")
    assertthat::assert_that(toupper(join) %in% c("INNER", "LEFT"),
                            msg = "Only inner and left joins are supported. Please choose one of 'inner' or 'left'.")

    suffix <- ""
    if (orig == TRUE)
        suffix <- "_orig"

    if (attr(d1, "level") != attr(d2, "level")) {
        .tsmsg(paste("d1 level =", attr(d1, "level")))
        .tsmsg(paste("d2 level =", attr(d2, "level")))
        stop("d1 and d2 are not the same level.")
    } else {
        level <- attr(d1, "level")
    }

    c1 <- getItem(d1, paste("counts", suffix, sep = "")) %>% as.data.frame
    g1 <- getItem(d1, paste("geneData", suffix, sep = ""))
    design1 <- getItem(d1, paste("design", suffix, sep = ""))

    c2 <- getItem(d2, paste("counts", suffix, sep = "")) %>% as.data.frame
    design2 <- getItem(d2, paste("design", suffix, sep = ""))

    id1 <- rownames(c1)
    id2 <- rownames(c2)

    switch(toupper(join),
           "INNER" = ids <- id1[id1 %in% id2],
           "LEFT" = ids <- id1
    )

    geneData <- g1[rownames(g1) %in% ids,]

    if (!any(colnames(design1) == colnames(design2)))
        stop('No common columns in design data!')

    if (any(rownames(design1) %in% rownames(design2))) {
        warning("SampleName clash.  Adding \"_2\" suffix to SampleNames in d2")
        rownames(design2) <- paste(rownames(design2), "_2", sep = "")
        colnames(c2) <- paste(colnames(c2), "_2", sep = "")
    }

    idx <- colnames(design1) %in% colnames(design2)
    commonColNames <- colnames(design1)[idx]
    design1 <- design1[,commonColNames]
    design2 <- design2[,commonColNames]
    design <- rbind(design1, design2)

    c1$.ID <- rownames(c1)
    c2$.ID <- rownames(c2)
    counts <- merge(x = c1, y = c2, by = ".ID", all.x = TRUE)

    rownames(counts) <- counts$.ID
    counts$.ID = NULL
    counts <- as.matrix(counts)

    idx <- sort(rownames(geneData), index.return = TRUE)$ix
    geneData <- geneData[idx,]
    idx <- sort(rownames(counts), index.return = TRUE)$ix
    counts <- counts[idx,]

    dgeObj <- initDGEobj(counts = counts,
                         rowData = geneData,
                         colData = design,
                         level = level)

    return(dgeObj)
}
