% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deployments_apiWrapper.R, R/Deployment.R
\name{GetDeployment}
\alias{GetDeployment}
\title{GetDeployment}
\usage{
GetDeployment(deploymentId, ...)
}
\arguments{
\item{deploymentId}{character. The ID of the deployment.}

\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\value{
A DataRobotDeployment object containing:
\itemize{
 \item id character. The ID of the deployment.
 \item label character. The label of the deployment.
 \item description character. The description of the deployment.
 \item defaultPredictionServer list. Information on the default prediction
   server connected with the deployment. See \code{ListPredictionServers}
   for details.
 \item model dataRobotModel. The model associated with the deployment.
   See \code{GetModel} for details.
 \item capabilities list. Information on the capabilities of the deployment.
 \item predictionUsage list. Information on the prediction usage of the deployment.
 \item permissions list. User's permissions on the deployment.
 \item serviceHealth list. Information on the service health of the deployment.
 \item modelHealth list. Information on the model health of the deployment.
 \item accuracyHealth list. Information on the accuracy health of the deployment.
}
}
\description{
Retrieve deployment
}
\details{
Retrieve a deployment.

This method invokes \verb{GET /deployments/\{deploymentId\}/} in the DataRobot Public API.

Get information on a particular deployment.
}
\examples{
\dontrun{
deploymentId <- "5e319d2e422fbd6b58a5edad"
GetDeployment(deploymentId)
}
}
\seealso{
The method DeploymentsRetrieve in \link[datarobot.apicore:DeploymentsApi]{datarobot.apicore::DeploymentsApi}, which invokes the same underlying API endpoint.
}
