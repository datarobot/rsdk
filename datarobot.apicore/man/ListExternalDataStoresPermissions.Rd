% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_connectivity_apiWrapper.R
\name{ListExternalDataStoresPermissions}
\alias{ListExternalDataStoresPermissions}
\title{ListExternalDataStoresPermissions}
\usage{
ListExternalDataStoresPermissions(dataStoreId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Describe data store permissions.
}
\details{
Describe what permissions current user has for given data store.

This method invokes \verb{GET /externalDataStores/\{dataStoreId\}/permissions/} in the DataRobot Public API.
}
\seealso{
The method ExternalDataStoresPermissionsList in \link{DataConnectivityApi}, which invokes the same underlying API endpoint.
}
