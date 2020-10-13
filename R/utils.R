#' DGEobj dimensions
#'
#' Reports the dimensions of the assay slot (row = genes; col = samples) in a DGEobj.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param x  A class DGEobj created by function initDGEobj()
#'
#' @return An integer vector [r,c] with a length of 2.
#'
#' @export
dim.DGEobj <- function(x) {
    dimension <- c(0, 0)

    idx <- attr(x, "basetype") == "assay"
    myassays <- unclass(x)[idx]

    if (length(myassays) > 0)
        dimension <- dim(myassays[[1]])

    return(dimension)
}


#' DGEobj dimnames
#'
#' Returns the rownames and colnames in a list of length 2 for a DGEobj.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param x  A class DGEobj created by function initDGEobj()
#'
#' @return A list of length 2 containing rownames and colnames of the DGEobj

#' @export
dimnames.DGEobj <- function(x){
    firstAssay <- getBaseType(x, "assay")[[1]]
    return(list(rownames = rownames(firstAssay), colnames = colnames(firstAssay)))
}


#' Function inventory
#'
#' Show the contents of a DGEobj object.  Note if an item is one-dimensional,
#' the row column reports the length.  This method is also invoked by applying
#' print() to a DGEobj.
#'
#' @author John Thompson
#' @keywords RNA-Seq
#'
#' @param dgeObj A DGEobj
#' @param verbose Add funArgs to the output (Default = FALSE)
#'
#' @return A data.frame summarizing the data contained in the DGEobj
#'
#' @examples
#' \dontrun{
#'   # Capture in a dataframe
#'   Mydf <- inventory(myDGEobj)
#'
#'   # Capture dataframe with verbose output
#'   Mydf <- inventory(myDGEobj, verbose=TRUE)
#' }
#'
#' @export
inventory <- function(dgeObj, verbose = FALSE)  {
    ItemNames <- names(dgeObj)
    ItemTypes <- attr(dgeObj, "type")
    BaseTypes <- attr(dgeObj, "basetype")
    Parents <- attr(dgeObj, "parent")
    creationDates <- attr(dgeObj, "dateCreated")
    FunArgs <- attr(dgeObj, "funArgs")
    Class <- lapply(dgeObj, class)
    Class <- lapply(Class, `[[`, 1)
    Row <- rep(NA, length(dgeObj))
    Col <- rep(NA, length(dgeObj))

    # Get length/dimensions
    for (i in 1:length(dgeObj)) {
        if (is.null(dim(dgeObj[[i]]))) {
            Row[i] <- length(dgeObj[[i]])
        } else {
            Dim <- dim(dgeObj[[i]])
            Row[i] <- Dim[1]
            Col[i] <- Dim[2]
        }
    }

    df <- data.frame(cbind(ItemName = ItemNames,
                           ItemType = ItemTypes,
                           BaseType = BaseTypes,
                           Parent = Parents,
                           Class = Class,
                           Row = Row,
                           Col = Col,
                           DateCreated = creationDates),
                     row.names = NULL)
    if (verbose == TRUE)
        df$FunArgs <- unlist(FunArgs)

    return(df)
}


#' Function print.DGEobj
#'
#' Print a DGEobj object.
#'
#' @author John Thompson
#' @keywords RNA-Seq
#'
#' @param x A DGEobj object
#' @param ...     Additional parameters
#' @param verbose Add funArgs to the output (Default = FALSE)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#'     print(myDGEobj)
#'     print(myDGEobj, verbose = TRUE)
#' }
#' @export
print.DGEobj <- function(x, ..., verbose = FALSE) {
    df <- inventory(x, verbose = verbose)
    print(df)
    return(invisible(x))
}


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
                            !missing(itemName),
                            length(itemName) == 1,
                            class(itemName)[[1]] == "character")

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
                            "DGEobj" %in% class(dgeObj))

    if ("list" %in% class(items)) items <- unlist(items)

    if (any(c("numeric", "integer") %in% class(items)) & max(items) > length(dgeObj))
        stop("A value in items numeric index is gt items in dgeObj" )

    if (any(c("numeric", "integer") %in% class(items))) items <- names(dgeObj)[items]

    for (item in items) {
        dgeObj <- rmItem(dgeObj, item)
    }

    return(dgeObj)
}


#' Function subset.DGEobj (DGEobj)
#'
#' This is the subset.DGEobj function bound to square brackets.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param x    A class DGEobj created by initDGEobj()
#' @param ...  Additional parameters
#' @param row  Row index for the subset
#' @param col  Col index for the subset
#' @param drop Included for compatibility but has no real meaning in the context
#'    of subsetting a DGEobj. So drop = FALSE is the default and changing this
#'    has no effect.
#' @param debug Default = FALSE. Set to TRUE to get more information if subsetting a
#'    DGEobj fails with a dimension error.
#'
#' @return A subsetted DGEobj class object
#'
#' @examples
#' \dontrun{
#'    DGEobj <- subset(DGEobj, 1:10, 100:1000)
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#'
#' @export
subset.DGEobj <- function(x, ..., row, col, drop = FALSE, debug = FALSE){

    assertthat::assert_that(class(x)[[1]] == "DGEobj")

    # Fill in missing row/col args
    if (missing(row))
        row <- 1:nrow(x)
    if (missing(col))
        col <- 1:ncol(x)

    # Make sure row and col in range
    if (class(row)[[1]] %in% c("numeric", "integer") & max(row) > nrow(x))
        stop("row coordinates out of range")
    if (class(col)[[1]] %in% c("numeric", "integer") & max(col) > ncol(x))
        stop("col coordinates out of range")

    # Warn if named items don't exist
    if (class(row)[[1]] == "character") {
        count <- length(row)
        foundcount <- sum(row %in% rownames(x))
        if (foundcount < count)
            warning(stringr::str_c((count - foundcount), " items in row index not found in rownames(x)"))
    }
    if (class(col)[[1]] == "character") {
        count <- length(col)
        foundcount <- sum(col %in% colnames(x))
        if (foundcount < count)
            warning(stringr::str_c((count - foundcount), " items in col index not found in colnames(x)"))
    }

    basetypes <- attr(x, "basetype")

    dropClasses <- c("data.frame", "matrix")

    if (class(row)[[1]] == "character")
        row <- rownames(x) %in% row
    if (class(col)[[1]] == "character")
        col <- colnames(x) %in% col

    for (i in 1:length(x)) {
        if (debug == TRUE) {
            cat(stringr::str_c("subsetting", names(x)[i], basetypes[[i]], "\n", sep = " "))
            cat(stringr::str_c("row arg length", length(row), class(row), "\n", sep = " "))
            cat(stringr::str_c("col arg length", length(col), class(col), "\n", sep = " "))
            cat(stringr::str_c("object dim: ", nrow(x[[i]]), ":", ncol(x[[i]])))
        }

        objectClass <- class(x[[i]])[[1]]

        userAttribs <- getAttributes(x[[i]])

        switch(basetypes[[i]],
               row = {
                   if (is.null(dim(x[[i]]))) {
                       x[[i]] <- x[[i]][row]
                   } else if (objectClass %in% dropClasses) {
                       x[[i]] <- x[[i]][row, , drop = drop]
                   } else {
                       x[[i]] <- x[[i]][row,]
                   }
               },

               col = {
                   if (is.null(dim(x[[i]]))) {
                       x[[i]] <- x[[i]][col]
                   } else if (objectClass %in% dropClasses) {
                       x[[i]] <- x[[i]][col, , drop = drop]
                   } else {
                       x[[i]] <- x[[i]][col,]
                   }
               },

               assay = {
                   if (objectClass %in% dropClasses) {
                       x[[i]] <- x[[i]][row, col, drop = drop]
                   } else {
                       x[[i]] <- x[[i]][row, col]
                   }
               })

        if (!"GRanges" %in% class(x[[i]]) & length(userAttribs) > 0)
            x[[i]] <- setAttributes(x[[i]], userAttribs)
    }

    return(x)
}


#' Shortcut function for subsetting a DGE object with square brackets.
#' Drop is supported for compatibility, but has no effect in this context.
#'
#' @param dgeObj A class DGEobj created by initDGEobj()
#' @param row Row index for the subset
#' @param col Col index for the subset
#' @param drop Included for compatibility but has no real meaning in the context
#'    of subsetting a DGEobj. So drop = FALSE is the default and changing this
#'    has no effect.
#' @param debug Default = FALSE. Set to TRUE to get more information if subsetting a
#'    DGEobj fails with a dimension error.
#'
#' @return A subsetted DGEobj class object
#'
#' @export
`[.DGEobj` <- function(dgeObj,
                       row,
                       col,
                       drop = FALSE,
                       debug = FALSE){
    dgeObj <- subset(dgeObj, row, col, drop, debug)
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

    assert_that(class(dgeObj) == "DGEobj")

    df <- as.data.frame(unlist(attr(dgeObj, "objDef")$type), stringsAsFactors = FALSE)
    df$type <- rownames(df)
    colnames(df) <- c("BaseType", "Type")
    df <- df[, c("Type", "BaseType")]
    if (pretty)
        knitr::kable(df, row.names = FALSE)
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

    assertthat::assert_that(!missing(dgeObj), !missing(itemType),
                            !missing(baseType), class(dgeObj) == "DGEobj",
                            baseType %in% baseTypes(dgeObj))

    attr(dgeObj, "objDef")$type[itemType] <- baseType
    if (uniqueItem == TRUE)
        attr(dgeObj, "objDef")$uniqueType <- c(attr(dgeObj, "objDef")$uniqueType, itemType)

    return(dgeObj)
}
