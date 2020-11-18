#' Function getItems
#'
#' Retrieve items from a DGEobj by item name.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class DGEobj created by function initDGEobj()
#' @param itemNames A character string, character vector, or list of itemNames to retrieve
#'
#' @return The requested data item or list of data items.
#'
#' @examples
#' \dontrun{
#'    MyCounts <- getItems(DGEobj, "counts")
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#'
#' @export
getItems <- function(dgeObj, itemNames){

    assertthat::assert_that(!missing(dgeObj),
                            !missing(itemNames),
                            msg = "Specify both a DGEobj and at least one itemName to retrieve.")
    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(any(c("character", "list") %in% class(itemNames)),
                            msg = "Pass the itemNames as a single character string, a character vector, or a list of string names to retrieve.")

    idx <- itemNames %in% names(dgeObj)
    result <- list()
    for (itemName in itemNames[idx]) {
        result[[itemName]] <- dgeObj[[itemName]]
    }

    if (length(result) == 1) result <- result[[1]]

    if (sum(idx) < length(idx)) {
        missingItems <- stringr::str_c(itemNames[!idx], sep = ", ")
        warning(stringr::str_c("These item(s) not found: [", missingItems, "]"))
    }

    return(result)
}


#' Function getItem
#'
#' Retrieve an item from a DGEobj by item name.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class DGEobj created by function initDGEobj()
#' @param itemName Name of item to retrieve
#'
#' @return The requested data item
#'
#' @examples
#' \dontrun{
#'    MyCounts <- getItem(DGEobj, "counts")
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
getItem <- function(dgeObj, itemName){
    assertthat::assert_that(!missing(dgeObj),
                            !missing(itemName),
                            msg = "Specify both a DGEobj and an itemName to retrieve.")
    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(class(itemName) == "character",
                            length(itemName) == 1,
                            msg = "The itemName should be a character string and contain the name of only one item to retrieve. To retrieve multiple items, use the getItems() function.")
    assertthat::assert_that(itemName %in% names(dgeObj),
                            msg = "The requested itemName should be in the DGEobj. Use names(dgeObj) to see the available items.")
    return(dgeObj[[itemName]])
}


#' Function getType
#'
#' Retrieve one or more data items from a DGEobj by type.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class DGEobj created by function initDGEobj()
#' @param type A single type of list of types to retrieve.  Enter
#'    showTypes(MyDGEobj) to see a list of allowed types.  See addType() function
#'    to define new types.
#' @param parent (Optional) Filter return list for common parent (e.g. useful
#' to select one set of contrast results when multiple fits have been performed)
#'
#' @return A list of requested data items
#'
#' @examples
#' \dontrun{
#'    MyContrastList <- getType(DGEobj, type = "topTable")
#'    MyRawData      <- getType(DGEobj, type = list("counts", "design", "geneData"))
#'}
#'
#' @export
getType <- function(dgeObj, type, parent){

    idx <- attr(dgeObj, "type") %in% type
    if (!missing(parent)) {
        pidx <- attr(dgeObj, "parent") == parent
        idx <- idx & pidx
    }
    result <- unclass(dgeObj)[idx]

    if (sum(idx) < length(type))
        warning("Some types were not found")

    if (sum(idx) == 0) {
        .tsmsg("Warning: no items of specified type are found.")
        return(NULL)
    } else {
        if (sum(idx) < length(type))
            warning("Some types were not found")
        return(result)
    }
}


#' Function getBaseType
#'
#' Accessor function for DGEobj class objects.  Retrieves all data items of a
#' given baseType or list of baseTypes.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class DGEobj created by function initDGEobj()
#' @param baseType One or more of: ["row", "col", "assay", "meta"]
#'
#' @return A simple list of data items
#'
#' @examples
#' \dontrun{
#'    Assays                  <- getBaseType(DGEobj, baseType = "assay")
#'    AssaysAndGeneAnnotation <- getBaseType(DGEobj, c("assay", "row"))
#' }
#'
#' @export
getBaseType <- function(dgeObj, baseType){

    if (missing(baseType))
        stop("baseType argument is required")

    if (!baseType %in% baseTypes(dgeObj))
        stop(paste("baseType must be one of: ",
                   paste(baseTypes(dgeObj), collapse = ", "),
                   sep = ""))

    idx <- attr(dgeObj, "basetype") %in% baseType

    if (sum(idx) < length(baseType))
        warning("Some baseTypes were not found")

    result <- unclass(dgeObj)[idx]
    return(result)
}
