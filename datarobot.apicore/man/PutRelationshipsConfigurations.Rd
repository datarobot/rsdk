% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{PutRelationshipsConfigurations}
\alias{PutRelationshipsConfigurations}
\title{PutRelationshipsConfigurations}
\usage{
PutRelationshipsConfigurations(
  relationships,
  featureDiscoverySettings,
  relationshipsConfigurationId,
  datasetDefinitions,
  featureDiscoveryMode,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Replace a relationships configuration
}
\details{
Replace a relationships configuration

This method invokes \verb{PUT /relationshipsConfigurations/\{relationshipsConfigurationId\}/} in the DataRobot Public API.
}
\seealso{
The method RelationshipsConfigurationsPut in \link{ProjectsApi}, which invokes the same underlying API endpoint.
}
