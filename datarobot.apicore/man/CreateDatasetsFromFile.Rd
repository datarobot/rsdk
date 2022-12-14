% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ai_catalog_apiWrapper.R
\name{CreateDatasetsFromFile}
\alias{CreateDatasetsFromFile}
\title{CreateDatasetsFromFile}
\usage{
CreateDatasetsFromFile(file, categories, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Create dataset from file
}
\details{
Create a dataset from a file.

This method invokes \verb{POST /datasets/fromFile/} in the DataRobot Public API.
}
\seealso{
The method DatasetsFromFileCreate in \link{AiCatalogApi}, which invokes the same underlying API endpoint.
}
