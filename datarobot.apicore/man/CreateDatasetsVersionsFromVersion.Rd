% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ai_catalog_apiWrapper.R
\name{CreateDatasetsVersionsFromVersion}
\alias{CreateDatasetsVersionsFromVersion}
\title{CreateDatasetsVersionsFromVersion}
\usage{
CreateDatasetsVersionsFromVersion(
  password,
  credentials,
  credentialId,
  datasetId,
  credentialData,
  categories,
  user,
  datasetVersionId,
  useKerberos = FALSE,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Create dataset version from previous version
}
\details{
Create a new version of the specified dataset from the specified dataset version. This will reuse the same source of the data that was previously used. Not supported for datasets that were previously loaded from an uploaded file. If the dataset is currently a remote dataset, it will be converted to a snapshot dataset. NOTE: If the specified version uses a Data Source, the user and password must be specified so the data can be accessed.

This method invokes \verb{POST /datasets/\{datasetId\}/versions/\{datasetVersionId\}/fromVersion/} in the DataRobot Public API.
}
\seealso{
The method DatasetsVersionsFromVersionCreate in \link{AiCatalogApi}, which invokes the same underlying API endpoint.
}
