% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deployments_apiWrapper.R
\name{GetDeploymentAccuracy}
\alias{GetDeploymentAccuracy}
\title{GetDeploymentAccuracy}
\usage{
GetDeploymentAccuracy(
  deploymentId,
  modelId = NULL,
  start = NULL,
  end = NULL,
  segmentAttribute = NULL,
  segmentValue = NULL,
  targetClasses = NULL,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve accuracy metric
}
\details{
Retrieve accuracy metric for a certain time period.

This method invokes \verb{GET /deployments/\{deploymentId\}/accuracy/} in the DataRobot Public API.
}
\seealso{
The method DeploymentsAccuracyList in \link{DeploymentsApi}, which invokes the same underlying API endpoint.
}
