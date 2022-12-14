% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ai_catalog_apiWrapper.R
\name{ListDatasets}
\alias{ListDatasets}
\title{ListDatasets}
\usage{
ListDatasets(
  orderBy,
  datasetVersionIds,
  category,
  offset = 0,
  limit = 100,
  filterFailed = "false",
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
List datasets
}
\details{
List all datasets accessible by the user.

This method invokes \verb{GET /datasets/} in the DataRobot Public API.
}
\seealso{
The method DatasetsList in \link{AiCatalogApi}, which invokes the same underlying API endpoint.
}
