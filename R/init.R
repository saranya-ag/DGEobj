#' Function initDGEobj
#'
#' Initializes DGEobj with base data (counts, gene annotation and sample annotation).
#'
#' @author John Thompson
#' @keywords RNA-Seq
#'
#' @param counts A count matrix or dataframe with row and colnames.
#' @param rowData  Gene, isoform or exon level annotation. Rownames must match
#'    rownames in count matrix.
#' @param colData A dataframe describing the experiment design. Rownames much match
#'  colnames(counts).
#' @param level One of "gene", "isoform", or "exon".
#' @param customAttr An optional (but highly recommended) named list of attributes
#'     to assign to the DGEobj.
#' @param allowShortSampleIDs  Using sequential integer rownames (even if typed as character)
#'   is discouraged and by default will abort the DGEobj creation. If you have a legitimate
#'   need to have short sample names composed of numeric characters, you can set this argument to TRUE.
#'   (Default = FALSE)
#' @param DGEobjDef An object definition. Defaults to the global DGEobj definition
#'     (.DGEobjDef) and you usually shouldn't change this unless you're customizing
#'     the object for new data types.
#'
#' @return A class DGEobj object
#'
#' @examples
#' \dontrun{
#'    # Initialize a DGEobj
#'    myDgeObj <- initDGEobj(counts = MyCounts,
#'                           rowData = MyGeneAnnotation,
#'                           colData = MyDesign,
#'                           level = "gene",
#'                           customAttr = list (Genome = "Mouse.B38",
#'                                              GeneModel = "Ensembl.R84"))
#' }
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#' @importFrom methods as
#'
#' @export
initDGEobj <- function(counts,
                       rowData,
                       colData,
                       level,
                       customAttr,
                       allowShortSampleIDs = FALSE,
                       DGEobjDef = .DGEobjDef
) {

    assert_that(!missing(counts),
                !missing(colData),
                !missing(rowData),
                !missing(level),
                is.matrix(counts) | is.data.frame(counts),
                level %in% DGEobjDef$allowedLevels,
                !is.null(rownames(counts)),
                !is.null(colnames(counts)),
                !is.null(rownames(rowData)),
                !is.null(rownames(colData))
    )

    assert_that(nrow(counts) == nrow(rowData),
                ncol(counts) == nrow(colData))

    #Rows
    if (!all(rownames(counts) == rownames(rowData))) {
        counts <- counts[order(rownames(counts)),]
        rowData <- rowData[order(rownames(rowData)),]
    }
    # Columns
    if (!all(colnames(counts) == rownames(colData))) {
        counts <- counts[,order(colnames(counts))]
        colData <- colData[order(rownames(colData)),]
    }

    assert_that(
        all(rownames(counts) == rownames(rowData)),
        all(colnames(counts) == rownames(colData))
    )

    if (!allowShortSampleIDs == TRUE) {
        suppressWarnings(
            test <- as.numeric(rownames(colData))
        )

        if (all(is.na(test)) == FALSE) {
            sampleCount <- nrow(colData)
            minchar <- nchar(as.character(sampleCount))
            maxchar <- max(sapply(rownames(colData), nchar))
            assert_that(maxchar > minchar,
                        msg = str_c("It looks like you have numeric sample IDs (design rownames).",
                                    "Please supply a more specific sample identifier. ",
                                    "Use allowShortSampleIDs = TRUE to explicitily override this restriction",
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

    result <- try({gr <- as(rowData, "GRanges")}, silent = TRUE)
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
