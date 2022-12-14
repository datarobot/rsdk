% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ai_catalog_apiWrapper.R
\name{DeleteDatasetsRelationships}
\alias{DeleteDatasetsRelationships}
\title{DeleteDatasetsRelationships}
\usage{
DeleteDatasetsRelationships(datasetRelationshipId, datasetId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Delete dataset relationship.
}
\details{
Delete a dataset relationship.

This method invokes \verb{DELETE /datasets/\{datasetId\}/relationships/\{datasetRelationshipId\}/} in the DataRobot Public API.
}
\seealso{
The method DatasetsRelationshipsDelete in \link{AiCatalogApi}, which invokes the same underlying API endpoint.
}
