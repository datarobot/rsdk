% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/governance_apiWrapper.R
\name{CreateChangeRequests}
\alias{CreateChangeRequests}
\title{CreateChangeRequests}
\usage{
CreateChangeRequests(changeRequestCreate, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Create Change Request.
}
\details{
Request changes for a supported product entity. For now, you can request changes for deployments only.

This method invokes \verb{POST /changeRequests/} in the DataRobot Public API.
}
\seealso{
The method ChangeRequestsCreate in \link{GovernanceApi}, which invokes the same underlying API endpoint.
}
