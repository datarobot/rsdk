% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{RetrieveCustomModelsVersionsConversions}
\alias{RetrieveCustomModelsVersionsConversions}
\title{RetrieveCustomModelsVersionsConversions}
\usage{
RetrieveCustomModelsVersionsConversions(
  customModelId,
  conversionId,
  customModelVersionId,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Get a given custom model conversion.
}
\details{
Get a given custom model conversion.

This method invokes \verb{GET /customModels/\{customModelId\}/versions/\{customModelVersionId\}/conversions/\{conversionId\}/} in the DataRobot Public API.
}
\seealso{
The method CustomModelsVersionsConversionsRetrieve in \link[datarobot.apicore:ModelsApi]{datarobot.apicore::ModelsApi}, which invokes the same underlying API endpoint.
}
