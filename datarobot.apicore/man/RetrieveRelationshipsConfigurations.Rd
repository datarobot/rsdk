% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{RetrieveRelationshipsConfigurations}
\alias{RetrieveRelationshipsConfigurations}
\title{RetrieveRelationshipsConfigurations}
\usage{
RetrieveRelationshipsConfigurations(relationshipsConfigurationId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve a relationships configuration
}
\details{
Retrieve a relationships configuration

This method invokes \verb{GET /relationshipsConfigurations/\{relationshipsConfigurationId\}/} in the DataRobot Public API.
}
\seealso{
The method RelationshipsConfigurationsRetrieve in \link{ProjectsApi}, which invokes the same underlying API endpoint.
}
