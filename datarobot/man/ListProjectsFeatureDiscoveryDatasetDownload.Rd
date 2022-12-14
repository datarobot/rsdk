% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{ListProjectsFeatureDiscoveryDatasetDownload}
\alias{ListProjectsFeatureDiscoveryDatasetDownload}
\title{ListProjectsFeatureDiscoveryDatasetDownload}
\usage{
ListProjectsFeatureDiscoveryDatasetDownload(datasetId, projectId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Download the project dataset with features added by feature discovery
}
\details{
Download the project dataset with features added by feature discovery

This method invokes \verb{GET /projects/\{projectId\}/featureDiscoveryDatasetDownload/} in the DataRobot Public API.
}
\seealso{
The method FeatureDiscoveryDatasetDownloadList in \link[datarobot.apicore:ProjectsApi]{datarobot.apicore::ProjectsApi}, which invokes the same underlying API endpoint.
}
