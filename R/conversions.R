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


#' Function convertDGEobj
#'
#' Casts a DGEobj class object as the specified data class.
#' Supports conversion to RangedSummarizedExperiment, Expression Set,
#' or simple list.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj, RangedSummarizedExperiment, ExpressionSet
#'
#' @param dgeObj  A class DGEobj created by function initDGEobj()
#' @param Class  One of "RangedSummarizedExperiment", "RSE", "ExpressionSet", "ES"
#'   (case insensitive)
#'
#' @return An object of the specified class
#'
#' @examples
#' \dontrun{
#'    MyRSE  <- convertDGEobj(DGEobj, "RSE")
#'    MyES   <- convertDGEobj(DGEobj, "ES")
#'    MyList <- convertDGEobj(DGEobj, "list")
#' }
#'
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom assertthat assert_that
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom methods as
#'
#' @export
convertDGEobj <- function(dgeObj, Class) {

    .toRSE <- function(counts, rowData, colData, meta){
        result <- try({gr <- methods::as(rowData, "GRanges")}, silent = TRUE)
        if (class(result) == "try-error")
            RSE = SummarizedExperiment(assays = list(counts = counts),
                                       rowData = rowData,
                                       colData = S4Vectors::DataFrame(colData),
                                       metadata = S4Vectors::SimpleList(meta))
        else
            RSE = SummarizedExperiment(assays = list(counts = counts),
                                       rowRanges = gr,
                                       colData = S4Vectors::DataFrame(colData),
                                       metadata = S4Vectors::SimpleList(meta))
    }

    .toES <- function(counts, rowData, colData, meta){
        RSE <- .toRSE(counts, rowData, colData, meta)
        ES <- methods::as(RSE, "ExpressionSet")
    }

    supportedClasses <- list("RANGEDSUMMARIZEDEXPERIMENT", "RSE",
                             "EXPRESSIONSET", "ES", "LIST")

    assertthat::assert_that(!missing(dgeObj),
                            !missing(Class),
                            msg = "Specify both a DGEobj and a desired Class to convert the DGEobj to.")
    assertthat::assert_that(class(Class) == "character",
                            msg = "Specify the Class via character string" )
    assertthat::assert_that(toupper(Class) %in% supportedClasses,
                            msg = 'The specified class must be one of "RangedSummarizedExperiment", "RSE", "ExpressionSet", or "ES".')

    allowedLevels <- attr(dgeObj, "objDef")$allowedLevels
    if (!attr(dgeObj, "level") %in% allowedLevels)
        stop(paste("Not a supported level. Supported levels = [",
                   paste(allowedLevels, collapse = ", "),
                   "]", sep = ""))

    counts <- getItem(dgeObj, "counts")
    colData <- getItem(dgeObj, "design")
    levelName <- paste(attr(dgeObj, "level"), "Data", sep = "")
    rowData <- S4Vectors::DataFrame(getItem(dgeObj, levelName))
    meta <- getBaseType(dgeObj, "meta")

    if (toupper(Class) %in% supportedClasses)
        result <- switch(toupper(Class),
                         "RSE" = .toRSE(counts, rowData, colData, meta),
                         "RANGEDSUMMARIZEDEXPERIMENT" = .toRSE(counts, rowData, colData, meta),
                         "ES" = .toES(counts, rowData, colData, meta),
                         "EXPRESSIONSET" = .toES(counts, rowData, colData, meta),
                         "LIST" = unclass(dgeObj))
    else
        stop(paste("DGEobj to ", Class, " not supported", sep = ""))

    return(result)
}


#' Function convertRSE
#'
#' Casts a RSE class object as a DGEobj.  Only captures count
#' data and the associated row and col annotation.
#'
#' @author John Thompson
#' @keywords RNA-Seq, DGEobj, SummarizedExperiment, ExpressionSet
#'
#' @param RSE  An object of class RangedSummarizedExperiment
#' @param Class Destination class (one of "DGEobj", "ES", or "ExpressionSet")
#' @param countsName If the name of the counts object is not "Counts", use
#'   this argument to tell the function the actual name of the counts assay in the RSE.
#'
#' @return An object of the specified class
#'
#' @examples
#' \dontrun{
#'    MyDGEobj <- convertRSE(MyRSE, "DGEobj")
#'    MyES     <- convertRSE(MyRSE, "ES")
#' }
#'
#' @import magrittr
#' @importFrom SummarizedExperiment rowRanges colData assay
#' @importFrom S4Vectors metadata
#' @importFrom methods as
#'
#' @export
convertRSE <- function(RSE,
                       Class,
                       countsName = "counts"){

    .toDGEobj <- function(RSE){
        counts <- SummarizedExperiment::assay(RSE, countsName)
        design <- SummarizedExperiment::colData(RSE) %>% as.data.frame
        geneAnnotation = SummarizedExperiment::rowRanges(RSE) %>% as.data.frame

        level <-  S4Vectors::metadata(RSE)[["Level"]] %>% tolower

        dgeObj <- initDGEobj(counts = counts,
                             rowData = geneAnnotation,
                             colData = design,
                             level = level)

        return(dgeObj)
    }

    assertthat::assert_that(!missing(RSE),
                            !missing(Class),
                            msg = "Specify both a RSE and a desired Class to convert the RSE to.")
    assertthat::assert_that(Class %in% c("DGEobj", "ES", "ExpressionSet"),
                            msg = 'The destination class must be one of "DGEobj", "ES", or "ExpressionSet".')
    assertthat::assert_that(class(RSE)[[1]] == "RangedSummarizedExperiment",
                            msg = "The RSE must be of class 'RangedSummarizedExperiment'.")

    result <- switch(toupper(Class),
                     "DGEOBJ" = .toDGEobj(RSE),
                     "ES" = methods::as(RSE, "ExpressionSet"),
                     "ExpressionSet" = methods::as(RSE, "ExpressionSet")
    )

    return(result)
}
