#' Function resetDGEobj
#'
#' During a workflow, a DGEobj typically gets filtered down to remove samples
#' that fail QC or non-expressed genes.  The resetDGEobj() function produces a new DGEobj with
#' the original unfiltered data.
#'
#' We can restore the original data because a copy of the original data was stored on the "meta"
#' layer and the "meta" layer is not subject to row or column filters.
#'
#' @author John Thompson
#' @keywords RNA-Seq; counts; low intensity
#'
#' @param dgeObj A DGEobj that we wish to extract original, un-filtered data from.
#'
#' @return A DGEobj containing the original (unfiltered) data.
#'
#' @examples
#' \dontrun{
#'     # Get back the original data, e.g. to start a different analysis
#'     unfilteredDGEobj <- resetDGEobj(MyHeavilyFilteredDGEobj)
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
resetDGEobj <- function(dgeObj){
    platform.types <- c("rna-seq", "rnaseq", "affymetrix")

    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(!is.null(attr(dgeObj, "level")),
                            msg = "The DGEobj must have a 'level' attribute specified.")
    assertthat::assert_that(!is.null(attr(dgeObj, "PlatformType")),
                            msg = "Required attribute \"PlatformType\" is missing.")

    platformType <- tolower(attr(dgeObj, "PlatformType"))
    counts       <- getItem(dgeObj, "counts_orig")
    design       <- getItem(dgeObj, "design_orig")

    if ("geneData_orig" %in% names(dgeObj)) {
        rowData <- getItem(dgeObj, "geneData_orig")
    } else if ("isoformData_orig" %in% names(dgeObj)) {
        rowData <- getItem(dgeObj, "isoformData_orig")
    } else if ("exonData_orig" %in% names(dgeObj)) {
        rowData <- getItem(dgeObj, "exonData_orig")
    } else if ("proteinData_orig" %in% names(dgeObj)) {
        rowData <- getItem(dgeObj, "proteinData_orig")
    } else {
        stop("Gene/isoform/exon/protein data not found")
    }

    if (tolower(platformType) %in% platform.types) {
        newObj <- initDGEobj(counts    = counts,
                             rowData   = rowData,
                             colData   = design,
                             level     = attr(dgeObj, "level"),
                             DGEobjDef = attr(dgeObj, "objDef"))
    } else {
        stop("The PlatformType attribute value was not recognized!")
    }

    if ("effectiveLength_orig" %in% names(dgeObj)) {
        newObj <- addItem(newObj,
                          item = dgeObj$effectiveLength_orig,
                          itemName = "effectiveLength_orig",
                          itemType = "effectiveLength_orig")

        newObj <- addItem(newObj,
                          item = dgeObj$effectiveLength_orig,
                          itemName = "effectiveLength",
                          itemType = "effectiveLength",
                          parent = "effectiveLength_orig")
    }

    excludeList <- list("names",
                        "class",
                        "row.names",
                        "dim",
                        "dimnames",
                        "objDef",
                        "type",
                        "itemName",
                        "itemType",
                        "basetype",
                        "parent",
                        "funArgs",
                        "level",
                        "dateCreated")
    attributes.dgeObj <- getAttributes(dgeObj, excludeList = excludeList)
    for (at in names(attributes.dgeObj)) {
        attr(newObj, at) <- attributes.dgeObj[[at]]
    }

    return(newObj)
}
