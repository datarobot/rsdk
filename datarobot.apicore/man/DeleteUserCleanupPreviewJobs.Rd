% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_management_apiWrapper.R
\name{DeleteUserCleanupPreviewJobs}
\alias{DeleteUserCleanupPreviewJobs}
\title{DeleteUserCleanupPreviewJobs}
\usage{
DeleteUserCleanupPreviewJobs(statusId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Cancel users perma-delete preview building.
}
\details{
Cancel scheduled users permanent delete preview building.

This method invokes \verb{DELETE /userCleanupPreviewJobs/\{statusId\}/} in the DataRobot Public API.
}
\seealso{
The method UserCleanupPreviewJobsDelete in \link{UserManagementApi}, which invokes the same underlying API endpoint.
}
