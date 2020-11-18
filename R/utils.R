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

#' Function as.list.DGEobj
#'
#' Casts a DGEobj class object as a simple list.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj
#'
#' @param x    A class DGEobj created by function initDGEobj()
#' @param ...  Additional parameters
#'
#' @return A simple list version of the DGEobj.
#'
#' @examples
#' \dontrun{
#'    MyDGElist <- as.list(DGEobj)
#' }
#'
#' @export
as.list.DGEobj <- function(x, ...){
    x2 <- unclass(x)
    return(x2)
}
