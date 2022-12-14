% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{GetFeatureAssociationMatrixDetails}
\alias{GetFeatureAssociationMatrixDetails}
\title{GetFeatureAssociationMatrixDetails}
\usage{
GetFeatureAssociationMatrixDetails(
  project,
  feature1,
  feature2,
  featurelistId = NULL,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieval for feature association plotting between a pair of features.
}
\details{
Retrieval for feature association plotting between a pair of features.   Projects created prior to v2.17 are not supported by this feature.

This method invokes \verb{GET /projects/\{projectId\}/featureAssociationMatrixDetails/} in the DataRobot Public API.
}
\seealso{
The method ProjectsFeatureAssociationMatrixDetailsList in \link{InsightsApi}, which invokes the same underlying API endpoint.
}
