% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deployments_apiWrapper.R
\name{SubmitActuals}
\alias{SubmitActuals}
\title{SubmitActuals}
\usage{
SubmitActuals(actuals, deploymentId, batchSize = 10000, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Submit actuals values
}
\details{
Submit actuals values for processing. Values are not processed immediately and may take some time to propagate through deployment systems. Submission of actuals is limited to 10,000,000 actuals per hour. For time series deployments, total actuals = number of actuals \\* number of forecast distances. For example, submitting 10 actuals for a deployment with 50 forecast distances = 500 total actuals. For multiclass deployments, a similar calculation is made where total actuals = number of actuals \\* number of classes. For example, submitting 10 actuals for a deployment with 20 classes = 200 actuals.

This method invokes \verb{POST /deployments/\{deploymentId\}/actuals/fromJSON/} in the DataRobot Public API.
}
\seealso{
The method DeploymentsActualsFromJSONCreate in \link{DeploymentsApi}, which invokes the same underlying API endpoint.
}
