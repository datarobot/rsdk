% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deployments_apiWrapper.R
\name{PatchDeploymentsChallengers}
\alias{PatchDeploymentsChallengers}
\title{PatchDeploymentsChallengers}
\usage{
PatchDeploymentsChallengers(
  deploymentId,
  name,
  challengerId,
  predictionEnvironmentId,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Update challenger model
}
\details{
Update challenger model.

This method invokes \verb{PATCH /deployments/\{deploymentId\}/challengers/\{challengerId\}/} in the DataRobot Public API.
}
\seealso{
The method DeploymentsChallengersPatch in \link[datarobot.apicore:DeploymentsApi]{datarobot.apicore::DeploymentsApi}, which invokes the same underlying API endpoint.
}
