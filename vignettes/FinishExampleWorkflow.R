# JRT 19Jan2021
# called from DGEobj_Overview.Rmd
# DGEobj building script
# We need an example DGEobj with a complete limma/voom workflow for the vignette examples.
# Take the annotated DGEobj from the vignette (DGEobj_Overview.Rmd) and complete the limma/voom workflow
# code culled from DGE.Tools2_Workflow.Rmd and modified to suppress typical plots and just produce the DGEobj.

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

FullWorkflowDGEobj <- dgeObj

projectName <- "GEO120804"
## Print columns available for building contrasts
designCol <- "ReplicateGroup"
levels <- str_c(designCol, (unique(FullWorkflowDGEobj$design[[designCol]])))
FullWorkflowDGEobj$design$ReplicateGroup %<>% as.factor()
FullWorkflowDGEobj$design$ReplicateGroup %<>% relevel("Sham")

### LowIntensityFilter

# Gene Filter Criteria
countThreshold <- 10  # Must meet or exceed this value
sampleFraction <- 0.5  # Fraction of samples that must meet the established criteria
# low expression filter
FullWorkflowDGEobj <- lowIntFilter(FullWorkflowDGEobj,
                       countThreshold = countThreshold,
                       sampleFraction = sampleFraction)

### filterProteinCoding

idx <- FullWorkflowDGEobj$geneData$gene_biotype == "protein_coding"
FullWorkflowDGEobj <- FullWorkflowDGEobj[idx,]

### Normalize

FullWorkflowDGEobj <- runEdgeRNorm(FullWorkflowDGEobj, plotFile = NULL)

### ModelDefinition

# Formula must be composed of column names from the design table.
formula <- '~ 0 + ReplicateGroup'
# User-defined name for the designMatrix
designMatrixName <- "ReplicateGroupDesign"

# build the designMatrix
design <- getItem(FullWorkflowDGEobj, "design")
designMatrix <- model.matrix (as.formula(formula), design)

# Make sure the column names in the design matrix are legal
# convert spaces and other disallowed chars to underscores or dots
colnames(designMatrix) <- make.names(colnames(designMatrix))

#capture the formula as an attribute of the design matrix
attr(designMatrix, "formula") <- formula

#add the designMatrix to the DGEobj
FullWorkflowDGEobj <- addItem(FullWorkflowDGEobj, item=designMatrix,
                  itemName=designMatrixName,
                  itemType="designMatrix",
                  parent="design",
                  overwrite=TRUE)

### runVoom
FullWorkflowDGEobj <- runVoom(FullWorkflowDGEobj, designMatrixName, mvPlot = FALSE)


### runContrasts\

# Name the design matrix to be used
designMatrixName <- "ReplicateGroupDesign"

##  Define the named contrasts from design matrix column names
contrastList  <- list(BDL_vs_Sham = "ReplicateGroupBDL - ReplicateGroupSham",
                      EXT1024_vs_BDL = "ReplicateGroupBDL_EXT.1024  - ReplicateGroupBDL",
                      Nint_vs_BDL = "ReplicateGroupBDL_Nint - ReplicateGroupBDL",
                      Sora_vs_BDL = "ReplicateGroupBDL_Sora - ReplicateGroupBDL"
)

FullWorkflowDGEobj <- runContrasts(FullWorkflowDGEobj,
                       designMatrixName=designMatrixName,
                       contrastList=contrastList,
                       Qvalue=TRUE,
                       IHW = TRUE)


