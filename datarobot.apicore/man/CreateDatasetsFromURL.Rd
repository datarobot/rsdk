% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ai_catalog_apiWrapper.R
\name{CreateDatasetsFromURL}
\alias{CreateDatasetsFromURL}
\title{CreateDatasetsFromURL}
\usage{
CreateDatasetsFromURL(
  persistDataAfterIngestion,
  categories,
  sampleSize,
  url,
  doSnapshot = TRUE,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Create dataset from URL
}
\details{
Create a Dataset Item from a URL.

This method invokes \verb{POST /datasets/fromURL/} in the DataRobot Public API.
}
\seealso{
The method DatasetsFromURLCreate in \link{AiCatalogApi}, which invokes the same underlying API endpoint.
}
