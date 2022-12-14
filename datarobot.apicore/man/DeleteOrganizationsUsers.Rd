% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_management_apiWrapper.R
\name{DeleteOrganizationsUsers}
\alias{DeleteOrganizationsUsers}
\title{DeleteOrganizationsUsers}
\usage{
DeleteOrganizationsUsers(organizationId, userId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Remove user from organization
}
\details{
Remove the user from the organization.

This method invokes \verb{DELETE /organizations/\{organizationId\}/users/\{userId\}/} in the DataRobot Public API.
}
\seealso{
The method OrganizationsUsersDelete in \link{UserManagementApi}, which invokes the same underlying API endpoint.
}
