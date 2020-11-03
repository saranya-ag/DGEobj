

#' Function rmItem
#'
#' Removes a named data item from a DGEobj.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj A class DGEobj created by initDGEobj()
#' @param itemName Name of the item to remove from the DGEobj
#'
#' @return An updated DGEobj
#'
#' @examples
#' \dontrun{
#'    MyDgeObj <- rmItem(MyDgeObj, "design")
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
rmItem <- function(dgeObj, itemName){

    assertthat::assert_that(class(dgeObj)[[1]] == "DGEobj",
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(!missing(itemName),
                            length(itemName) == 1,
                            class(itemName)[[1]] == "character",
                            msg = "Specify a singular itemName as a character string.")

    if (!itemName %in% names(dgeObj))
        stop(paste(itemName, " does not exist within DGEresult.", sep = ""))

    dgeObj[itemName] <- NULL

    attr(dgeObj, "basetype")[itemName] <- NULL
    attr(dgeObj, "type")[itemName] <- NULL
    attr(dgeObj, "parent")[itemName] <- NULL
    attr(dgeObj, "funArgs")[itemName] <- NULL
    attr(dgeObj, "dateCreated")[itemName] <- NULL

    return(dgeObj)
}


#' Function rmItems
#'
#' Removes a vector or list of named data items from a DGEobj.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj A class DGEobj created by function initDGEobj()
#' @param items Either a character vector of item names or a numeric index of items to
#'   remove from the DGEobj Use inventory(DGEobj) to view the indexes of items.
#'
#' @return An updated DGEobj
#'
#' @examples
#' \dontrun{
#'    MyDgeObj <- rmItems(MyDgeObj, c("designMatrix", "designMatrix_Elist"))
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
rmItems <- function(dgeObj, items){
    assertthat::assert_that(!missing(dgeObj),
                            !missing(items),
                            msg = "Specify both a DGEobj and a character vector or list of items to remove.")
    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The DGEobj must be of class 'DGEobj'.")

    if ("list" %in% class(items)) items <- unlist(items)

    if (any(c("numeric", "integer") %in% class(items)) & max(items) > length(dgeObj))
        stop("A value in items numeric index is gt items in dgeObj" )

    if (any(c("numeric", "integer") %in% class(items))) items <- names(dgeObj)[items]

    for (item in items) {
        dgeObj <- rmItem(dgeObj, item)
    }

    return(dgeObj)
}
