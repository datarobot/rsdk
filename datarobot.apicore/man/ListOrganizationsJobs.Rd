% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_management_apiWrapper.R
\name{ListOrganizationsJobs}
\alias{ListOrganizationsJobs}
\title{ListOrganizationsJobs}
\usage{
ListOrganizationsJobs(organizationId, offset = 0, limit = 0, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
List organization jobs
}
\details{
List currently running jobs belonging to this organization.

This method invokes \verb{GET /organizations/\{organizationId\}/jobs/} in the DataRobot Public API.
}
\seealso{
The method OrganizationsJobsList in \link{UserManagementApi}, which invokes the same underlying API endpoint.
}
