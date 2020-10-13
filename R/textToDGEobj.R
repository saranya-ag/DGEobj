#' Function  textToDGEobj
#'
#' Builds a DGEobj from Omicsoft output files.
#'
#' The input files may be gzipped, however, in that case you'll need to provide
#' the filenames in the "counts", "seqAnnotation" and "design" arguments.
#'
#' Data Format Requirements:
#' This function is designed for Omicsoft output files:
#'
#' Count data should have rownames (sequence ids) and column names (sample IDs).
#' Sequence annotation should also have the same rownames as assays.
#'
#' If possible, the sequence annotation should include chromosome position data (chr,
#' start, end, strand).
#'
#' Sample annotation must have one row for each column in the count table.
#' rownames(sampleAnnotation) == colnames(counts).
#'
#' Function DGEobj::annotateDGEobj reads key=value pairs from a text file to define
#' attributes.
#'
#' For sequence annotation: The gene ID is used as the
#' row name.  The following fields are also included: "GeneName" (GeneSymbol),
#' "Transcript." (transcript#), "Strand", "Chromosome", "Start", "End",
#' "ExonLength" (sum of exons), "Source", "IsConfounded"
#' (overlaps another gene).
#'
#' Certain downstream functionality depends on specific sequence annotation
#' fields.  For example, Chromosome, Start, End and Strand are required to build
#' a GenomicRanges object.  And many people depend on the source field to filter
#' for protein-coding genes.  So you can probably build a DGEobj with any old annotation file.
#' But minimally adhering to these columns of data will insure operability with downstream
#' tools.
#'
#' @author John Thompson
#' @keywords Omicsoft, DGEObj, RNA-Seq, Data loading
#'
#' @param path Path for the three data files (required).  For cloud, use s3fs to
#'   mount a S3 bucket to a local folder path.
#' @param counts A matrix or dataframe of R genes by C samples (required)
#'  [Default = "RNA-Seq.Count.Table.txt"]
#' @param seqAnnotation  Gene, isoform, or exon level (row) annotation (required)
#'  [Default = "RNA-Seq.Count.Annotation.txt"]
#' @param design Sample annotation with expt factors and other sample-associated
#'     data (required) [Default = RNA-Seq.Design.txt"]
#' @param level One of "gene", "isoform", "exon" (required)
#' @param customAttr A named list of custom attributes to attach to the DGEobj;
#'    Optional but highly encouraged
#' @param gz Set to true to automatically add ".gz" to the default file names.  This
#'   is a convenience for when the default files are gzipped.
#' @param verbose Output more info while reading data (Default = FALSE)
#'
#' @return A DGEobj
#'
#' @examples
#' \dontrun{
#'     dgeObj <- textToDgeObj(path = "path/to/omicsoft/files", level = "gene", verbose = TRUE)
#' }
#' @importFrom stringr str_c
#' @importFrom assertthat assert_that
#' @importFrom utils packageVersion read.table
#'
#' @export
textToDGEobj <- function(path,
                         counts = "RNA-Seq.Count.Table.txt",
                         seqAnnotation = "RNA-Seq.Count.Annotation.txt",
                         design = "RNA-Seq.Design.txt",
                         level,
                         customAttr,
                         gz=FALSE,
                         verbose=FALSE){

  assertthat::assert_that(!missing(path),
                          !missing(level),
                          tolower(level) %in% c("gene", "isoform", "exon"))

   # Change default filenames if not given and level = isoform
    if (tolower(level) == "isoform") {
        if (missing(counts))
            counts <- stringr::str_c("RNA-Seq.Transcript_Count.Table.txt")
        if (missing(seqAnnotation))
            seqAnnotation <- stringr::str_c("RNA-Seq.Transcript_Count.Annotation.txt")
        if (missing(design))
            design <- stringr::str_c("RNA-Seq.Design.txt")
    }

    # Add convenience support for gzipped files
    if (gz == TRUE) {
      gz <- ".gz"
    } else {
      gz <- ""
    }
    if (!str_detect(counts, ".gz$")) counts <- stringr::str_c(counts, gz)
    if (!str_detect(seqAnnotation, ".gz$")) seqAnnotation <- stringr::str_c(seqAnnotation, gz)
    if (!str_detect(design, ".gz$")) design <- stringr::str_c(design, gz)

    # Get the data
    if (verbose) tsmsg("Reading count data...")
    if (file.exists(file.path(path, counts))) {
      countData <- Txt2DF(file.path(path, counts))
    } else {
      stop(str_c("Couldn't find file: ", counts))
    }

    if (verbose) tsmsg("Reading seq annotation data...")
    if (file.exists(file.path(path, seqAnnotation))) {
      seqData <- Txt2DF(file.path(path, seqAnnotation))
    } else {
      stop(str_c("Couldn't find file: ", seqAnnotation))
    }

    if (verbose) tsmsg("Reading seq annotation data...")
    if (file.exists(file.path(path, design))) {
      designData <- Txt2DF(file.path(path, design))
    } else {
      stop("Couldn't find file: ", design)
    }

    rownames(designData) <- make.names(rownames(designData))

    if (missing(customAttr)) customAttr <- list()

    # Add DGE.Tools2Version info
    customAttr$DGEobj <- utils::packageVersion("DGEobj")

    # Build the DgeObj
    DgeObj <- initDGEobj(counts = countData,
                         rowData = seqData,
                         colData = designData,
                         level,
                         customAttr = customAttr)

    return(DgeObj)
}


### Function Txt2DF ###
Txt2DF <- function(filename) {
  # Configured to read Omicsoft .txt files correctly capturing GeneIDs as rownames
  if (file.exists(filename)) {
    df = utils::read.table(filename,
                           sep = "\t",
                           stringsAsFactors = FALSE,
                           header = TRUE,
                           row.names = 1,
                           comment.char = "",
                           quote = "",
                           na.strings = c("NA", "."),
                           check.names = TRUE)
    return(df)
  } else {
    warning(paste("Warning: File = ", filename, "not found."))
    return(-1)
  }
}

### Function tsmsg ###
# A timestamped message
tsmsg <- function(...) {
  # Works like message() but prepends a timestamp
  message(date(), ": ", ...)
}
