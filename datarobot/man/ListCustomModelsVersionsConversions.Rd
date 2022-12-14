% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{ListCustomModelsVersionsConversions}
\alias{ListCustomModelsVersionsConversions}
\title{ListCustomModelsVersionsConversions}
\usage{
ListCustomModelsVersionsConversions(
  customModelId,
  customModelVersionId,
  isLatest = "false",
  offset = 0,
  limit = 1000,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Get a list or latest custom model conversion(s).
}
\details{
Get the list of custom model conversions that are associated with the given custom model. Alternatively, it can return a single item list of the latest custom model conversion that is associated with the given custom model version.

This method invokes \verb{GET /customModels/\{customModelId\}/versions/\{customModelVersionId\}/conversions/} in the DataRobot Public API.
}
\seealso{
The method CustomModelsVersionsConversionsList in \link[datarobot.apicore:ModelsApi]{datarobot.apicore::ModelsApi}, which invokes the same underlying API endpoint.
}
