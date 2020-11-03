#' Function BaseType
#'
#' Return the baseType for a given item type in a DGEobj.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj A class DGEobj created by function initDGEobj()
#' @param type  An item type for which you want the baseType
#'
#' @return A baseType value (character string)
#'
#' @examples
#' \dontrun{
#'    MyBaseType <- baseType(dgeObj, type = "DGEList")
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
baseType <- function(dgeObj, type){

    assertthat::assert_that(!missing(dgeObj),
                            !missing(type),
                            msg = "Specify both a DGEobj and a type (to check the baseType). Both are required.")
    assertthat::assert_that(class(dgeObj)[[1]] == "DGEobj",
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(class(type)[[1]] == "character",
                            msg = "The type must be of class 'character'.")

    objDef <- attr(dgeObj, "objDef")
    return(objDef$type[[type]])
}


#' Function baseTypes
#'
#' Return a list of the available baseTypes in a DGEobj.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class DGEobj object
#'
#' @return A list of baseTypes
#'
#' @examples
#' \dontrun{
#'    # Global definition of baseTypes
#'    myBaseTypes <- baseTypes()
#'
#'    # Basetypes from a specific DGEobj
#'    myBaseTypes <- baseTypes(myDGEobj)
#' }
#'
#' @export
baseTypes <- function(dgeObj){
    if (missing(dgeObj))
        return(unique(.DGEobjDef$type))
    else
        return(unique(attr(dgeObj, "objDef")$type))
}


#' Function showTypes
#'
#' Show the list of all Types defined in the DGEobj.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class DGEobj created by function initDGEobj()
#' @param pretty TRUE (the default) invokes knitr::kable() to print a
#'    nicely formatted table
#'
#' @return Prints a list of defined "types"
#'
#' @examples
#' \dontrun{
#'    showTypes(MyDGEobj)
#' }
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#' @importFrom knitr kable
#'
#' @export
showTypes <- function(dgeObj, pretty = TRUE){

    assertthat::assert_that(class(dgeObj) == "DGEobj",
                            msg = "The DGEobj must be of class 'DGEobj'.")

    df <- as.data.frame(unlist(attr(dgeObj, "objDef")$type), stringsAsFactors = FALSE)
    df$type <- rownames(df)
    colnames(df) <- c("BaseType", "Type")
    df <- df[, c("Type", "BaseType")]
    if (pretty)
        knitr::kable(df, row.names = FALSE)
    else
        df
}


#' Function newType
#'
#' Used to customize a DGEobj definition by adding new types.  A baseType
#' is also declared and whether more than a single instance is allowed.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj A class DGEobj created by initDGEobj()
#' @param itemType The name of the new type to create
#' @param baseType The baseType of the new item. One of [row, col, assay, meta]
#' @param uniqueItem If set to TRUE, only one instance of the new type is
#'    allowed in a DGEobj
#'
#' @return A DGEobj with a new type definition embedded
#'
#' @examples
#' \dontrun{
#'     MyDgeObj <- newType(MyDgeObj,
#'                         itemType   = "AffyRMA",
#'                         baseType   = "assay",
#'                         uniqueItem = TRUE)
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
newType <- function(dgeObj, itemType, baseType, uniqueItem = FALSE){

    result <- FALSE

    assertthat::assert_that(!missing(dgeObj),
                            !missing(itemType),
                            !missing(baseType),
                            msg = "Specify the DGEobj, itemType, and baseType. All three are required.")
    assertthat::assert_that(class(dgeObj) == "DGEobj",
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(baseType %in% baseTypes(dgeObj),
                            msg = "The baseType must be one of the baseTypes available in the DGEobj. Use baseTypes(DGEobj) to see which are available.")

    attr(dgeObj, "objDef")$type[itemType] <- baseType
    if (uniqueItem == TRUE)
        attr(dgeObj, "objDef")$uniqueType <- c(attr(dgeObj, "objDef")$uniqueType, itemType)

    return(dgeObj)
}
