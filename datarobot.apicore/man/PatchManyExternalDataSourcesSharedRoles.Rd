% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_connectivity_apiWrapper.R
\name{PatchManyExternalDataSourcesSharedRoles}
\alias{PatchManyExternalDataSourcesSharedRoles}
\title{PatchManyExternalDataSourcesSharedRoles}
\usage{
PatchManyExternalDataSourcesSharedRoles(dataSourceId, roles, operation, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Modify data source shared roles.
}
\details{
Grant access, remove access or update roles for organizations, groups or users on this data source. Up to 100 roles may be set per array in a single request.

This method invokes \verb{PATCH /externalDataSources/\{dataSourceId\}/sharedRoles/} in the DataRobot Public API.
}
\seealso{
The method ExternalDataSourcesSharedRolesPatchMany in \link{DataConnectivityApi}, which invokes the same underlying API endpoint.
}
