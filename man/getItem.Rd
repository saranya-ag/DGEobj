% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{getItem}
\alias{getItem}
\title{Retrieve a data item by name}
\usage{
getItem(dgeObj, itemName)
}
\arguments{
\item{dgeObj}{A DGEobj}

\item{itemName}{Name of item to retrieve}
}
\value{
The requested data item
}
\description{
Retrieve a data item by name
}
\examples{
    # example DGEobj
    exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))

    MyCounts <- getItem(exObj, "counts")

}
