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
#' @param platformType One of "RNA-Seq" or "Affymetrix". Only required if the
#' platformType attribute is missing from the DGEobj.
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
#' @importFrom stringr str_c
#'
#' @export
resetDGEobj <- function(dgeObj, platformType){
    platform.rnaseq <- c("rna-seq", "rnaseq")

    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            !is.null(attr(dgeObj, "level")))

    if (is.null(attr(dgeObj, "PlatformType")))
        stop("Required attribute \"PlatformType\" is missing!  Must use platformType argument.")

    metaList <- getBaseType(dgeObj, "meta")[1:3]

    if (!is.null(attr(dgeObj, "PlatformType")))
        platformType <- tolower(attr(dgeObj, "PlatformType"))

    if (tolower(platformType) %in% platform.rnaseq)
        counts <- getItem(dgeObj, "counts_orig")

    design <- getItem(dgeObj, "design_orig")

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

    if (tolower(platformType) %in% platform.rnaseq) {
        newObj <- initDGEobj(counts = dgeObj$counts_orig,
                             rowData = rowData,
                             colData = design,
                             level = attr(dgeObj, "level"),
                             DGEobjDef = attr(dgeObj, "objDef")
        )
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
