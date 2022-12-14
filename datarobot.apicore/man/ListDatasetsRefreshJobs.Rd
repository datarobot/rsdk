% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ai_catalog_apiWrapper.R
\name{ListDatasetsRefreshJobs}
\alias{ListDatasetsRefreshJobs}
\title{ListDatasetsRefreshJobs}
\usage{
ListDatasetsRefreshJobs(datasetId, offset = 0, limit = 100, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Information about scheduled jobs for given dataset.
}
\details{
Paginated list of scheduled jobs descriptions for a specific dataset with given dataset ID, sorted by time of the last update.

This method invokes \verb{GET /datasets/\{datasetId\}/refreshJobs/} in the DataRobot Public API.
}
\seealso{
The method DatasetsRefreshJobsList in \link{AiCatalogApi}, which invokes the same underlying API endpoint.
}
