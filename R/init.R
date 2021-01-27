#' Initialize with base data (counts, gene annotations, sample annotations)
#'
#' @param counts               A count matrix or dataframe with row and
#'   colnames. Each column represents a sample.  Each row represents and asssy
#'   (gene).
#' @param rowData              Gene, isoform or exon level annotation. Rownames
#'   must match the rownames in count matrix
#' @param colData              A dataframe describing the experiment design.
#'   Rownames much match the colnames(counts)
#' @param level                One of "gene", "isoform", or "exon"
#' @param customAttr           (optional) Named list of attributes
#' @param allowShortSampleIDs  Using sequential integer rownames (even if typed
#'   as character) is discouraged and by default will abort the DGEobj creation.
#'   If you have a legitimate need to have short sample names composed of
#'   numeric characters, you can set this argument to TRUE (default = FALSE)
#' @param DGEobjDef            An object definition. Defaults to the global
#'   DGEobj definition (initDGEobjDef()) and you usually shouldn't change this unless
#'   you're customizing the object for new data types.
#'
#' @return A DGEobj
#'
#' @examples
#'
#'    dgeObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj", mustWork = TRUE))
#'    MyCounts <- dgeObj$counts
#'    geneinfo <- dgeObj$geneData
#'    sampinfo <- dgeObj$design
#'
#'   myDgeObj <- initDGEobj(counts = MyCounts,
#'                          rowData = geneinfo,
#'                          colData = sampinfo,
#'                          level = "gene",
#'                          customAttr = list (Genome = "Rat.B6.0",
#'                                             GeneModel = "Ensembl.R89"))
#'
#' @import magrittr
#' @importFrom GenomicRanges GRanges
#' @importFrom assertthat assert_that
#'
#' @export
initDGEobj <- function(counts,
                       rowData,
                       colData,
                       level,
                       customAttr,
                       allowShortSampleIDs = FALSE,
                       DGEobjDef = initDGEobjDef()
) {

    assertthat::assert_that(!missing(counts),
                            !missing(colData),
                            !missing(rowData),
                            !missing(level),
                            msg = "Specify the counts, colData, rowData, and level. All are required to initialize a DGEobj.")
    assertthat::assert_that(is.matrix(counts) | is.data.frame(counts),
                            msg = "counts must be specified as a matrix or a data.frame.")
    assertthat::assert_that(level %in% DGEobjDef$allowedLevels,
                            msg = 'The specified level must be one of: "gene", "isoform", "exon", "proteingroup", "peptide", "ptm", or "protein".')
    assertthat::assert_that(!is.null(rownames(counts)),
                            !is.null(colnames(counts)),
                            !is.null(rownames(rowData)),
                            !is.null(rownames(colData)),
                            msg = "counts must have row and column names specified. rowData and colData must have rownames specified.")
    assertthat::assert_that(nrow(counts) == nrow(rowData),
                            ncol(counts) == nrow(colData),
                            msg = "The number of rows in counts must match the number of rows in rowData. Similarly, the number of columns in counts must match the number of columns in colData.")

    # Rows
    if (!all(rownames(counts) == rownames(rowData))) {
        counts <- counts[order(rownames(counts)),]
        rowData <- rowData[order(rownames(rowData)),]
    }
    # Columns
    if (!all(colnames(counts) == rownames(colData))) {
        counts <- counts[,order(colnames(counts))]
        colData <- colData[order(rownames(colData)),]
    }

    assertthat::assert_that(
        all(rownames(counts) == rownames(rowData)),
        all(colnames(counts) == rownames(colData)),
        msg = "The rownames for counts must match the rownames of rowData. Similarly, the colnames of counts must match the rownames of colData."
    )

    if (!allowShortSampleIDs == TRUE) {
        suppressWarnings(
            test <- as.numeric(rownames(colData))
        )

        if (all(is.na(test)) == FALSE) {
            sampleCount <- nrow(colData)
            minchar <- nchar(as.character(sampleCount))
            maxchar <- max(sapply(rownames(colData), nchar))
            assertthat::assert_that(maxchar > minchar,
                                    msg = paste("It looks like you have numeric sample IDs (design rownames).",
                                                "Please supply a more specific sample identifier. ",
                                                "Use allowShortSampleIDs = TRUE to explicitly override this restriction",
                                                sep = "\n"))
        }
    }

    funArgs <- match.call()

    result <- try(counts <- as.matrix(counts), silent = TRUE)
    if (any(class(result) == "try-error"))
        stop("Couldn't coerce counts to a numeric matrix!")

    # Initialize an empty DGEobj
    dgeObj <- list()
    class(dgeObj) <- "DGEobj"
    attr(dgeObj, "objDef") <- DGEobjDef

    # Add empty attributes
    attr(dgeObj, "type") <- list()
    attr(dgeObj, "basetype") <- list()
    attr(dgeObj, "parent") <- list()
    attr(dgeObj, "funArgs") <- list()
    attr(dgeObj, "dateCreated") <- list()

    # Load required items
    # Counts
    dgeObj <- addItem(dgeObj,
                      item = counts,
                      itemName = "counts_orig",
                      itemType = "counts_orig",
                      funArgs = funArgs,
                      parent = "",
                      init = TRUE
    )

    dgeObj <- addItem(dgeObj,
                      item = counts,
                      itemName = "counts",
                      itemType = "counts",
                      funArgs = funArgs,
                      parent = "counts_orig",
                      init = TRUE
    )

    # colData
    dgeObj <- addItem(dgeObj,
                      item = colData,
                      itemName = "design_orig",
                      itemType = "design_orig",
                      funArgs = funArgs,
                      parent = "")

    dgeObj <- addItem(dgeObj,
                      item = colData,
                      itemName = "design",
                      itemType = "design",
                      funArgs = funArgs,
                      parent = "design_orig")

    # rowData
    level <- tolower(level)
    switch(level,
           "gene" = itemName <- "geneData",
           "isoform" = itemName <- "isoformData",
           "exon" = itemName <- "exonData"
    )
    itemType <- itemName
    parent <- paste(itemName, "_orig", sep = "")
    grparent <- itemName

    dgeObj <- addItem(dgeObj,
                      item = rowData,
                      itemName = parent,
                      itemType = parent,
                      funArgs = funArgs,
                      parent = "")

    dgeObj <- addItem(dgeObj,
                      item = rowData,
                      itemName = itemName,
                      itemType = itemType,
                      funArgs = funArgs,
                      parent = parent)

    # Annotate the level
    dgeObj %<>% setAttributes(list(level = level))

    result <- try({gr <- GenomicRanges::GRanges(rowData)}, silent = TRUE)

    if (class(result) == "try-error") {
        warning("Couldn't build a GRanges object!")
    } else {
        dgeObj <- addItem(dgeObj,
                          item = gr,
                          itemName = "granges_orig",
                          itemType = "granges_orig",
                          funArgs = funArgs,
                          parent = paste(grparent, "_orig", sep = ""))

        dgeObj <- addItem(dgeObj,
                          item = gr,
                          itemName = "granges",
                          itemType = "granges",
                          funArgs = funArgs,
                          parent = grparent)
    }

    if (!missing(customAttr))
        dgeObj <- setAttributes(dgeObj, customAttr)

    return(dgeObj)
}


#' Instantiate a class DGEobjDef object.
#'
#' @param types                A named character vector of new types where the values indicate the basetype (optional)
#' @param levels               A name or vector of names for new levels (optional)
#' @param uniqueTypes          A name or vector of names to add to the uniqueType list (optional)
#'
#' @return A class DGEobjDef object suitable for use with initDGEobj
#'
#' @examples
#'     # return the default DGEobj definition
#'     myDGEobjDef <- initDGEobjDef()
#'
#'     # Optionally add some new types and levels for metabolomics data
#'     myDGEobjDef <- initDGEobjDef(
#'                         types <- c(MSQuant = "assay"),
#'                         levels <- "metabolites",
#'                         uniqueTypes <- "MSQuant"
#'                         )
#'
#'
#' @importFrom assertthat assert_that
#'
#' @export
initDGEobjDef <- function(types, levels, uniqueTypes){

    newDef <- DGEobj:::.DGEobjDef

    if(!missing(types)){
        assertthat::assert_that("character" %in% class(types),
                                msg = "types argument must be a named character vector.")
        assertthat::assert_that(!is.null(names(types)),
                                msg = "The types vector must have names.")
        #only new types allowed, reject if type name already exists
        assertthat::assert_that(!any(names(types) %in% names (newDef$type)),
            msg = "At least one of the new types already exists.")

        #all type values must be a basetype
        assertthat::assert_that(all(types %in% newDef$basetype),
            msg = paste("The type values must be one of:", paste(newDef$basetype, collapse = " ")))

        #add the new type(s)
        newDef$type <- c(newDef$type, types)
    }

    if (!missing(levels)){
        assertthat::assert_that("character" %in% class(levels),
                                msg = "levels must be a character string or vector")
        assertthat::assert_that(!any(levels %in% newDef$allowedLevels),
                                msg = "Abort. Level already exists.")
        #add the new level(s)
        newDef$allowedLevels <- c(newDef$allowedLevels, levels)
    }

    if (!missing(uniqueTypes)){
        assertthat::assert_that("character" %in% class(uniqueTypes),
                                msg = "uniqueTypes must be a character string or vector")
        assertthat::assert_that(all(uniqueTypes %in% names(newDef$type)),
                                msg =  "Not a valid type.")
        #add them and remove dups
        newDef$uniqueType <- unique(c(newDef$uniqueType, uniqueTypes))
    }

    return(newDef)
}

