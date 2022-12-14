% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobs_apiWrapper.R
\name{DeleteStatus}
\alias{DeleteStatus}
\title{DeleteStatus}
\usage{
DeleteStatus(statusId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Delete a task
}
\details{
Destroy an async status object

This method invokes \verb{DELETE /status/\{statusId\}/} in the DataRobot Public API.
}
\seealso{
The method StatusDelete in \link{JobsApi}, which invokes the same underlying API endpoint.
}
