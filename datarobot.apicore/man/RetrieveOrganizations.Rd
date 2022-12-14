% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_management_apiWrapper.R
\name{RetrieveOrganizations}
\alias{RetrieveOrganizations}
\title{RetrieveOrganizations}
\usage{
RetrieveOrganizations(organizationId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve organization
}
\details{
Retrieve the organization details.

This method invokes \verb{GET /organizations/\{organizationId\}/} in the DataRobot Public API.
}
\seealso{
The method OrganizationsRetrieve in \link{UserManagementApi}, which invokes the same underlying API endpoint.
}
