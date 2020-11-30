# DGEobj: An S3 data object to capture results from Differential Gene Expression analysis

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/DGEobj?color=9bc2cf)](https://cran.r-project.org/package=DGEobj) <!--
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/grand-total/DGEobj?color=9bc2cf)](https://cran.r-project.org/package=DGEobj) -->
[![Travis build status](https://travis-ci.org/cb4ds/DGEobj.svg?branch=develop)](https://travis-ci.org/cb4ds/DGEobj?branch=develop)
[![Codecov test coverage](https://codecov.io/gh/cb4ds/DGEobj/branch/develop/graph/badge.svg)](https://codecov.io/gh/cb4ds/DGEobj?branch=develop)
<!-- badges: end -->

DGEobj is an S3 data class that provides a flexible container for Differential Gene Expression (DGE) analysis results.  The DGEobj class is designed to be extensible allowing definition of new data types as needed. A set of accessory functions to deposit, query and retrieve subsets of a data workflow has been provided.  Attributes are used to capture metadata such as species and gene model, including reproducibility information such that a 3rd party can access a DGEobj history to see how each data object was created or modified. 

Conceptually, the DGEobj borrows some concepts from the RangedSummarizedExperiment (RSE).  The DGEobj has data slots for row data (typically gene), column data (samples), assays (anything with n rows (observations) by m columns (samples) dimensions) and metadata (anything that can't be keyed to row, col or assay).  The key motivation for creating a new data structure is that the RSE only allows one data item each in the row and col slots and thus is unsuitable for capturing the various of data objects created during a typical DGE workflow.   The DGEobj data structure can hold any number of row and col data objects and thus is engineered for capturing the multiple steps of a downstream analysis.

Certain data types, primarily the count matrix, which typically represents the starting point of an analysis, and the associated row and column annotation, are defined as "unique" objects and thus only one instance of that data type may be added to the DGEobj.  

The concept of parent-child relationships in a workflow is used to associate downstream data objects with upstream data objects.  This enables a single DGEobj to capture a multithreaded workflow with multiple fits, each with is own contrasts.

### Structure of a DGEobj

Operationally, a DGE object is fundamentally a list of data objects. For example, in a typical RNA-Seq workflow, data objects that are useful to capture as documentation of the analysis might include normalization factors, design matrices, fit objects, and contrast output. Each data object deposited in a DGE object is accompanied by a set of attributes.  These attributes define the type of data that object represents as well as its parent object, date created.  

To allow for storing multiple row and col objects, we've defined a data "type" and "baseType". There are four fundamental and immutable baseTypes: **row, col, assay, meta**.  Pre-defined and user-defined "types" are assigned a "baseType".  

For convenience, pre-defined "types" include many types encountered in RNA-Seq analysis or proteomics analysis.  To provide flexibility, the newType function provides the ability to create new data types as needed.  Each type is associated with a baseType. 

A DGE object may contain multiple instances of a single data "type" unless the "unique" attribute has been set to disallow multiple instances of a data "type". Each instance of a data "type" is given a name, which distinguishes multiple instances of a single data "type".  A list of User-defined data type definitions are stored within the data object itself as an attribute. 

Each item stored in a DGE object also has a "funArgs" attribute.  A user-defined text string comment can be assigned to this attribute to provide additional details about the creation of a workflow item.  However, when items are being added to a DGE object within the context of a function,  assigning match.call() to the funArgs attribute captures the function call and thus automates capture of the function arguments in effect when the item was created.   
  
### Supporting functions include:  

#### Manipulate DGEObj  

* **initDGEobj**: Initialize a new DGEobj - requires counts, design, and gene (or isoform or exon) data  
* **addItem, addItems**:  Add one or multiple data items to a DGEobj  
* **rmItem, rmItems**:  Remove one or multiple data items from a DGEobj  
* **newType**:  Define a new data type  
* **setAttribute, setAttributes**: Allows setting one or multiple attributes  

#### Query Functions  

* **dim**: Return the dimensions of the DGEobj (the assay dimensions)  
* **dimnames**:  Return the row (gene) and column (sample) names  
* **showTypes**:  Show the type definitions of a DGEobj (all currently defined   types)  
* **inventory**:  Print a summary of the contents of a DGEobj, date created and optionally the funArgs history  

#### Data retrieval  

* **getItem, getItems**:  Return one or more data item(s) by item name (or a list of names)  
* **getType**:  Return data item(s) by item type  
* **getBaseType**:  Return data item(s) by baseType  
* **getAttribute, getAttributes**: Return attributes  

#### Conversion

* **as.list**:  unclass a DGEobj to simple list

#### GRanges Data

If the gene data object (row annotation) contains chromosome position information (chromosome, start, end, strand), a GRanges object will also be created. 

#### Original Data 

During DGE object initialization, a copy of the counts, gene annotation and sample annotation is duplicated and stored in the meta slot with an "_orig" suffix on the itemName.  This preserves the original data after subsetting and provides a means to extract the original unsubsetted data if needed.  
