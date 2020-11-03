#' Function showAttributes
#'
#' Prints the attributes associated with a DGEobj.  This prints
#' all attributes regardless of the class of the attribute value.
#' Use showMeta() if you are only interested in attributes that are
#' key/value pairs.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A DGEobj created by function initDGEobj()
#' @param skipList  A character vector of attributes to skip. Use this to avoid
#'   printing certain lengthy attributes like rownames.  Defaults to c("dim",
#'   dimnames", "rownames", "colnames", "listData", "objDef")
#'
#' @return Prints a list of attributes and values.
#'
#' @examples
#' \dontrun{
#'    showAttributes(myDGEobj)
#' }
#'
#' @export
showAttributes <- function(dgeObj,
                           skipList = c("dim", "dimnames", "rownames",
                                        "colnames", "listData", "objDef")) {

    at <- attributes(dgeObj)
    if (length(at) > 0) {
        print(names(at))
    }

    for (i in 1:length(dgeObj)) {
        dataName <- names(dgeObj)[i]
        print(paste("dataName", ":", sep = ""))

        atnames <- names(attributes(dgeObj[[i]]))
        atnames <- atnames[!(atnames %in% skipList)]
        print(paste("atnames:", paste(atnames, collapse = ", "), sep = " "))

        for (j in atnames)
            print(paste("[", j, "] = ", attr(dgeObj[[i]], j)))
    }
}


#' Function setAttributes
#'
#' Set one or more attributes on an object.  You can use this to add attribute
#' annotation(s) to a DGEobj or to a specific item within a DGEobj.
#' The setAttributes() function adds the attributes passed to it in
#' the attribs argument without deleting attributes already present. To remove an attribute,
#' you can pass NULL as the value. Any named attribute on the attribs argument list
#' that already exists in the object will be updated.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param item  An object to attach attributes to
#' @param attribs A named list of attribute/value pairs
#'
#' @return The item with new attributes added and no existing attributes removed
#'
#' @examples
#' \dontrun{
#'    # Assign attributes to a DGEobj
#'    MyAttributes <- list(Platform       = "RNA-Seq",
#'                         Instrument     = "HiSeq",
#'                         Vendor         = "Unknown",
#'                         readType       = "PE",
#'                         readLength     = 75,
#'                         strandSpecific = TRUE)
#'    MyDGEobj <- setAttributes(MyDGEobj, MyAttributes)
#'
#'    # Set attributes on an item inside a DGEobj
#'    MyAttributes <- list(normalized   = FALSE,
#'                         LowIntFilter = "FPK >5 in >= 1 group")
#'    MyDGEObj[["counts"]] <- setAttributes(MyDGEObj[["counts"]], MyAttributes)
#' }
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#'
#' @export
setAttributes <- function(item, attribs){

    assertthat::assert_that(!missing(item),
                            !missing(attribs),
                            msg = "Specify both an item and the attributes (attribs) to be attached to the item.")
    assertthat::assert_that(class(attribs)[[1]] == "list",
                            msg = "attribs must be of class 'list'.")
    assertthat::assert_that(!is.null(names(attribs)),
                            msg = "The attribs list should be a named list, specifying the attribute/value pairs. It must have names specified.")

    attribNames <- as.list(names(attribs))
    for (i in 1:length(attribs))
        item <- setAttribute(item, attribs[[i]], attribNames[[i]])
    return(item)
}


#' Function setAttribute
#'
#' Set an attribute on an object.  You can use this to add attribute
#' annotation(s) to a DGEobj or to a specific item within a DGEobj.
#'
#' @param item  An object to attach attributes to
#' @param attrib An attribute value to add to the item
#' @param attribName A name for the attribute
#'
#' @return The item with the new attribute added and no existing attributes removed
#'
#' @examples
#' \dontrun{
#'    # Assign attribute to a DGEobj
#'    MyDGEobj <- setAttribute(MyDGEobj, "RNA-SEQ", "Platform")
#'
#'    # Set attributes on an item inside a DGEobj
#'    MyDGEObj[["counts"]] <- setAttribute(MyDGEObj[["counts"]], FALSE, "normalized")
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
setAttribute <- function(item, attrib, attribName) {

    assertthat::assert_that(!missing(item),
                            !missing(attrib),
                            !missing(attribName),
                            msg = "Specify an item, the attribute (attrib), and the name of the attribute (attribName) to be attached to the item.")
    assertthat::assert_that(class(attribName) == "character",
                            msg = "attribName must be of class 'character'.")

    attr(item, attribName) <- attrib
    return(item)
}

#' Function getAttributes
#'
#' Get all attributes from an item except for any listed in the excludeList argument.
#' This is intended to capture user-defined attributes by excluding a few standard
#' attributes like class and dim.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param item  A DGEobj (or any object with attributes)
#' @param excludeList A list of attribute names to exclude from the output
#'     (default = list("dim", "dimnames", "names", "row.names"))
#'
#' @return A named list of attribute values for the items
#'
#' @examples
#' \dontrun{
#'    # Get attributes from a DGEobj
#'    MyAttr <- getAttributes(DGEobj)
#'
#'    # Get the formula attribute from the designMatrix
#'    MyAttr <- attr(DGEobj$designMatrixName, "formula")
#' }
#'
#' @export
getAttributes <- function(item,
                          excludeList = list("dim", "dimnames",
                                             "names", "row.names", "class")){
    at <- attributes(item)
    idx <- !names(at) %in% excludeList
    return(at[idx])
}


#' Function getAttribute
#'
#' Get a specified attribute from an item.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param item  A DGEobj or item
#' @param attrName Name of the attribute to retrieve.
#'
#' @return The specified attribute value (data type depends on the data type
#' stored in the attribute) or NULL if attribute doesn't exist
#'
#' @examples
#' \dontrun{
#'    # Get an attribute from a DGEobj
#'    MyAttr <- getAttribute(DGEobj, "type")
#'
#'    # Get an attribute from a DGEobj item
#'    MyAttr <- getAttribute(DGEobj$designMatrix, "formula")
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
getAttribute <- function(item, attrName){
    assertthat::assert_that(!missing(item),
                            !missing(attrName),
                            msg = "An item and an attribute name (attrName) are required.")

    x <- attr(item, attrName)
    return(x)
}


#' Function showMeta
#'
#' Prints the attributes associated with an object with a limit on the length of
#' the values stored in the attributes.  Use this to examine the key/value metadata
#' associated with a DGEobj.  Written for use with a DGEobj, but
#' should function generically on any object with key/value pair attributes.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param obj An object with attributes to examine (often a DGEobj created by function initDGEobj())
#'
#' @return A data frame with the key value pairs from the object's attributes.
#'
#' @examples
#' \dontrun{
#'    df <- showMeta(MyDGEobj)
#' }
#'
#' @importFrom utils stack
#'
#' @export
showMeta <- function(obj) {
    alist <- attributes(obj)

    idx <- lapply(alist, length) == 1

    if (sum(idx) > 0) {
        suppressWarnings(
            df <- utils::stack(alist[idx])
        )
        colnames(df) <- c("Value", "Attribute")
        df <- df[, c("Attribute", "Value")]
        df$Attribute <- as.character(df$Attribute)
        return(df)
    } else {
        return(NULL)
    }
}
