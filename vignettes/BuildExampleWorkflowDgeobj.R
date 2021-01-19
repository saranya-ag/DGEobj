# DGEobj building script
#  Build a DGEobj based on a standard limma/voom workflow
# code culled from DGE.Tools2_Workflow.Rmd

library(tidyverse)
library(magrittr)
library(DGEobj)
library(DGE.Tools2)
library(variancePartition)
library(Rtsne)
library(parallel)
library(doParallel)
library(glue)
library(biomaRt)
library(knitr)
library(conflicted)

# This determines the path to the git root.
setwd (here::here())
## set relative path to the markdown subfolder.
setwd("./vignettes")

## Set to the desired output destination
outputPath <- "./output"
if (!file.exists(outputPath)) dir.create(outputPath, recursive=TRUE)

### downloadProjectData

# Get the raw counts and sample annotation ("design") from GEO
# Source: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE120804
getLocation <- "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE120nnn/GSE120804/suppl"
countsFile <- "GSE120804_counts.txt.gz"
designFile <- "GSE120804_geo_sample_annotation_edit.csv.gz"
counts_url <- glue("{getLocation}/{countsFile}")
design_url <- glue("{getLocation}/{designFile}")

temp <- tempfile()
if (download.file(counts_url, destfile = temp, mode = 'wb') > 0) print ("Counts Download Failed")
counts <- read.delim(temp, stringsAsFactors = FALSE, row.names = 1)
unlink(temp)

temp <- tempfile()
if (download.file(design_url, destfile = temp, mode = 'wb')) print("Design Download Failed")
design <- read.csv(temp, stringsAsFactors = FALSE)
unlink(temp)

### cleanUpDesignTable

rownames(design) <- str_sub(design$raw.file, start = 1, end = 21)

#correct the desired case/spelling of one column
design %<>% dplyr::rename(ReplicateGroup = Replicate.group)

# Let's also create (parse) a design column to indicate whether a sample was
# from a BSL or sham animal
design$DiseaseStatus <- rep("Sham", nrow(design))
idx <- str_detect(design$ReplicateGroup, "BDL")
design$DiseaseStatus[idx] <- "BDL"

# Create an animal# column.  The animal number is encoded in the sample.name
# column.  Each animal's liver produces multiple slices and we'll which to
# account for this in our modeling.
design$AnimalNum <- str_match(design$Sample.name, "r[0-9]{1,3}")


### getGeneAnnotation

# Now let's get the gene annotation from Ensembl/biomaRt
ens.ds      <- "rnorvegicus_gene_ensembl"
ens.mart    <- useMart(biomart = "ensembl", dataset = ens.ds)
ens.columns <- c("ensembl_gene_id", "rgd_symbol", "chromosome_name", "start_position",
                 "end_position", "strand", "gene_biotype", "description")
ens.data    <- getBM(attributes = ens.columns, values = rownames(counts), mart = ens.mart) %>%
    distinct(ensembl_gene_id, .keep_all = T)

# Filter the list to the genes used in the test dataset and properly format gene
# information for GenomicRanges use
gene.data <- left_join(data.frame(ensembl_gene_id = rownames(counts), stringsAsFactors = F),
                       ens.data,
                       by = "ensembl_gene_id") %>%
    dplyr::rename(start = start_position, end = end_position) %>%
    mutate(strand = case_when(strand == -1 ~ "-",
                              strand == 1  ~ "+",
                              TRUE         ~ "*"))
rownames(gene.data) <- gene.data$ensembl_gene_id


### initializeDGEobj

dgeObj <- DGEobj::initDGEobj(counts  = counts,
                             rowData = gene.data,
                             colData = design,
                             level = "gene",
                             customAttr = list(Genome    = "Rat.B6.0",
                                               GeneModel = "Ensembl.R89"))

### annotateFromTextFile

annotationFile <- system.file("GSE120804_ProjectAttributes.txt", package = "DGE.Tools2", mustWork = TRUE)
dgeObj <- annotateDGEobj(dgeObj, annotationFile)



projectName <- "GEO120804"
## Print columns available for building contrasts
designCol <- "ReplicateGroup"
levels <- str_c(designCol, (unique(dgeObj$design[[designCol]])))
dgeObj$design$ReplicateGroup %<>% as.factor()
dgeObj$design$ReplicateGroup %<>% relevel("Sham")

### LowIntensityFilter

# Gene Filter Criteria
countThreshold <- 10  # Must meet or exceed this value
sampleFraction <- 0.5  # Fraction of samples that must meet the established criteria
# low expression filter
dgeObj <- lowIntFilter(dgeObj,
                       countThreshold = countThreshold,
                       sampleFraction = sampleFraction)

### filterProteinCoding

idx <- dgeObj$geneData$gene_biotype == "protein_coding"
dgeObj <- dgeObj[idx,]

### Normalize

dgeObj <- runEdgeRNorm(dgeObj, plotFile = file.path(outputPath, "TMM_NormFactors.PNG"))

### ModelDefinition

# Formula must be composed of column names from the design table.
formula <- '~ 0 + ReplicateGroup'
# User-defined name for the designMatrix
designMatrixName <- "ReplicateGroupDesign"

# build the designMatrix
design <- getItem(dgeObj, "design")
designMatrix <- model.matrix (as.formula(formula), design)

# Make sure the column names in the design matrix are legal
# convert spaces and other disallowed chars to underscores or dots
colnames(designMatrix) <- make.names(colnames(designMatrix))

#capture the formula as an attribute of the design matrix
attr(designMatrix, "formula") <- formula

#add the designMatrix to the DGEobj
dgeObj <- addItem(dgeObj, item=designMatrix,
                  itemName=designMatrixName,
                  itemType="designMatrix",
                  parent="design",
                  overwrite=TRUE)

### runVoom
dgeObj <- runVoom(dgeObj, designMatrixName)


### runContrasts\

# Name the design matrix to be used
designMatrixName <- "ReplicateGroupDesign"

##  Define the named contrasts from design matrix column names
contrastList  <- list(BDL_vs_Sham = "ReplicateGroupBDL - ReplicateGroupSham",
                      EXT1024_vs_BDL = "ReplicateGroupBDL_EXT.1024  - ReplicateGroupBDL",
                      Nint_vs_BDL = "ReplicateGroupBDL_Nint - ReplicateGroupBDL",
                      Sora_vs_BDL = "ReplicateGroupBDL_Sora - ReplicateGroupBDL"
)

dgeObj <- runContrasts(dgeObj,
                       designMatrixName=designMatrixName,
                       contrastList=contrastList,
                       Qvalue=TRUE,
                       IHW = TRUE)

saveRDS (dgeObj, file.path(outputPath, "WorkflowDgeObj.RDS"))
