% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deployments_apiWrapper.R
\name{ListDeploymentsModelSecondaryDatasetConfiguration}
\alias{ListDeploymentsModelSecondaryDatasetConfiguration}
\title{ListDeploymentsModelSecondaryDatasetConfiguration}
\usage{
ListDeploymentsModelSecondaryDatasetConfiguration(deploymentId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve secondary datasets configuration for a deployment.
}
\details{
Retrieve the secondary datasets configuration used by a deployed Feature discovery model.

This method invokes \verb{GET /deployments/\{deploymentId\}/model/secondaryDatasetConfiguration/} in the DataRobot Public API.
}
\seealso{
The method DeploymentsModelSecondaryDatasetConfigurationList in \link[datarobot.apicore:DeploymentsApi]{datarobot.apicore::DeploymentsApi}, which invokes the same underlying API endpoint.
}
