% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_connectivity_apiWrapper.R
\name{UpdateDataStore}
\alias{UpdateDataStore}
\title{UpdateDataStore}
\usage{
UpdateDataStore(
  dataStoreId,
  canonicalName = NULL,
  driverId = NULL,
  jdbcUrl = NULL,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Updates a data store configuration.
}
\details{
Updates a data store configuration.

This method invokes \verb{PATCH /externalDataStores/\{dataStoreId\}/} in the DataRobot Public API.
}
\seealso{
The method ExternalDataStoresPatch in \link{DataConnectivityApi}, which invokes the same underlying API endpoint.
}
