% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{GetFeatureAssociationMatrix}
\alias{GetFeatureAssociationMatrix}
\title{GetFeatureAssociationMatrix}
\usage{
GetFeatureAssociationMatrix(
  project,
  associationType = "association",
  metric = "mutualInfo",
  featurelistId = NULL,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieval for pairwise feature association statistics.
}
\details{
Retrieval for pairwise feature association statistics.   > Projects created prior to v2.17 are not supported by this feature.

This method invokes \verb{GET /projects/\{projectId\}/featureAssociationMatrix/} in the DataRobot Public API.
}
\seealso{
The method ProjectsFeatureAssociationMatrixList in \link{InsightsApi}, which invokes the same underlying API endpoint.
}
