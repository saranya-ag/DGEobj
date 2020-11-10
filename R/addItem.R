#' Function addItem (DGEobj)
#'
#' Add a data item to a class DGEobj
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class DGEobj created by function initDGEobj()
#' @param item  The data item to be deposited in the DGEobj (Required)
#' @param itemName A user assigned name for this data item. (Required)
#' @param itemType A type attribute.  See showTypes() to see the
#'     predefined types. Types are extensible with the newType() function. (Required)
#' @param overwrite Default = FALSE.  Set to TRUE to overwrite the data object
#'     stored in the itemName slot
#' @param funArgs A text field to annotate how the data object was created.
#'    If you pass the result of match.call() as this argument, it captures the
#'    name and arguments used in the current function (optional)
#' @param itemAttr A named list of attributes to add directly to the item (optional)
#' @param parent itemName of the parent of this item (optional, but your DGEobj
#'   won't be well annotated if you don't use this wherever appropriate)
#' @param init Default = FALSE. Used internally by the initDGEobj() function.
#' @param debug Default = FALSE; TRUE trigger browser mode.
#'
#' @return A DGEobj class object with a new data item added.
#'
#' @examples
#' \dontrun{
#'    myFunArgs <- match.call() # Capture calling function and arguments
#'    showTypes()  # See what predefined types are available
#'    myDGEobj <- addItem(myDGEobj, item = MyCounts,
#'                                  itemName = "counts",
#'                                  itemType = "counts",
#'                                  funArgs = myFunArgs)
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#'
#' @export
addItem <- function(dgeObj,
                    item, itemName, itemType,
                    overwrite = FALSE,
                    funArgs = match.call(),
                    itemAttr,
                    parent = "",
                    init = FALSE,
                    debug = FALSE) {

    # helper functions
    .dimensionMatch <- function(dgeObj, item, itemType){
        testrow <- function(dgeObj, item){
            if (is.null(dim(item))) {
                if (nrow(dgeObj) > 0 & nrow(dgeObj) != length(item))
                    stop('New row object does not match row dimension of DGEobj object')
            } else if (nrow(dgeObj) > 0 & nrow(dgeObj) != nrow(item))
                stop('New row object does not match row dimension of DGEobj object')
        }

        testcol <- function(dgeObj, item){
            if (is.null(dim(item))) {
                if (ncol(dgeObj) > 0 & ncol(dgeObj) != length(item))
                    stop('Rows in new col object must match col dimension of DGEobj object')
            } else if (ncol(dgeObj) > 0 & ncol(dgeObj) != nrow(item))
                stop('Rows in new col object must match col dimension of DGEobj object')
        }

        testassayrowcol <- function(dgeObj, item){
            if (nrow(dgeObj) > 0 & nrow(dgeObj) != nrow(item))
                stop('New assay object does not match row dimension of DGEobj object')
            if (ncol(dgeObj) > 0 & ncol(dgeObj) != ncol(item))
                stop('New assay object does not match col dimension of DGEobj object')
        }

        result <- FALSE
        switch(attr(dgeObj, "objDef")$type[[itemType]],
               "row" = testrow(dgeObj, item),
               "col" = testcol(dgeObj, item),
               "assay" = testassayrowcol(dgeObj, item))
        result <- TRUE
        return(result)
    }

    .checkDimnames <- function(dgeObj, item, basetype){
        result <- TRUE
        result <- switch(basetype,
                         col = all(rownames(item) == colnames(dgeObj)),
                         row = all(rownames(item) == rownames(dgeObj)),
                         assay = all(rownames(item) == rownames(dgeObj)) &
                             all(colnames(item) == colnames(dgeObj)),
                         meta = TRUE
        )
        return(result)
    }

    assertthat::assert_that(!missing(dgeObj),
                            !missing(item),
                            !missing(itemName),
                            !missing(itemType),
                            msg = "Specify the DGEobj, item, itemName, and itemType. All are required.")

    if (debug == TRUE) browser()

    allowedTypes <- names(attr(dgeObj, "objDef")$type)
    if (!itemType %in% allowedTypes) {
        stop(paste("itemType must be one of: ",
                   paste(allowedTypes, collapse = ", "), sep = ""))
    } else {
        basetype <- baseType(dgeObj, type = itemType)
    }

    switch(basetype,
           row = {if (!itemType == "granges" && is.null(rownames(item)))
               stop("Row basetypes must have rownames")},
           col = {if (is.null(rownames(item)))
               stop("Col basetypes must have rownames")},
           assay = {if (is.null(rownames(item)) || is.null(colnames(item)))
               stop("Assay basetypes must have row and column names")}
    )

    if (overwrite == FALSE & itemName %in% names(dgeObj))
        stop(stringr::str_c('itemName (', itemName, ') already exists in DGEobj!'))

    uniqueTypes <- attr(dgeObj, "objDef")$uniqueType
    if (itemType %in% uniqueTypes  &
        itemType %in% attr(dgeObj, "type") &
        overwrite == FALSE)
        stop(paste("Only one instance of type ", itemType, " allowed.",
                   " Use a base type instead (row, col, assay, meta),",
                   " or define a new type.", sep = ""))

    if (class(funArgs) == "call")
        funArgs <- paste(funArgs[[1]], "(",
                         paste(funArgs[2:length(funArgs)], collapse = ", "),
                         ")", sep = "")

    if (init == FALSE) {
        if (.dimensionMatch(dgeObj, item, itemType) == FALSE)
            stop(stringr::str_c("item doesn't match dimension of DGEobj [", itemName, "]"))

        if (!.checkDimnames(dgeObj, item = item, basetype = basetype))
            stop("item row and/or column names out of order with DGEobj")
    }

    if (!missing("itemAttr"))
        item <- setAttributes(item, itemAttr)

    dgeObj[[itemName]] <- item

    stdAttr <- list(
        type = itemType,
        basetype = basetype,
        dateCreated = as.character(Sys.time()),
        funArgs = funArgs,
        parent = parent
    )

    for (at in names(stdAttr)) {
        myattr <- attr(dgeObj, at)
        myattr[[itemName]] <- stdAttr[[at]]
        attr(dgeObj, at) <- myattr
    }

    return(dgeObj)
}


#' Function addItems (DGEobj)
#'
#' Add a data item to a class DGEobj
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A DGEobj that items will be added to. (Required)
#' @param itemList  A list of data items to add to DGEobj (Required)
#' @param itemTypes A list of type values for each item on itemList (Required)
#' @param parents A list of parent values for each item on itemList (Optional, but highly recommended)
#' @param overwrite Default = FALSE.  Set to TRUE to overwrite the data object
#'     stored in the itemName slot
#' @param itemAttr An named list of attributes to add to each item (Optional). The
#'    same set of attributes will be added to each item.
#'
#' @return A class DGEobj object with new items added.
#'
#' @examples
#' \dontrun{
#'    # Replace a set of contrasts after adding something to each
#'    myDGEobj <- addItems(myDGEobj, myContrastList, overwrite= TRUE)
#' }
#'
#' @importFrom assertthat assert_that
#'
#' @export
addItems <- function(dgeObj,
                     itemList, itemTypes,
                     parents,
                     overwrite = FALSE,
                     itemAttr) {

    assertthat::assert_that(!missing(dgeObj),
                            !missing(itemList),
                            !missing(itemTypes),
                            msg = "Specify the DGEobj, itemList, and itemTypes. All are required.")
    assertthat::assert_that(class(dgeObj)[[1]] == "DGEobj",
                            class(itemList)[[1]] == "list",
                            class(itemTypes)[[1]] == "list",
                            msg = "The DGEobj must be of class DGEobj, while the itemList and itemTypes must both be lists.")
    assertthat::assert_that(length(itemList) == length(itemTypes),
                            msg = "The length of the itemList must match the length of the itemTypes.")

    if (!missing(parents))
        assertthat::assert_that(class(parents)[[1]] == "list",
                                length(parents) == length(itemList),
                                msg = "The parents list must be of class 'list' and of the same length as the itemList.")

    if (!missing(itemAttr)) {
        attrNames <- names(itemAttr)
        for (i in 1:length(itemList))
            for (j in 1:length(itemAttr))
                attr(itemList[[i]], attrNames[[j]]) <- itemAttr[[j]]
    }

    if (missing(parents))
        parents <- as.list(rep("", length(itemList)))

    itemNames <- names(itemList)
    for (i in 1:length(itemList)) {
        dgeObj <- addItem(dgeObj,
                          item = itemList[[i]],
                          itemName = itemNames[[i]],
                          itemType = itemTypes[[i]],
                          parent = parents[[i]],
                          overwrite = overwrite)
    }

    return(dgeObj)
}
