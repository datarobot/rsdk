% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{CreateCustomModelsVersionsFromRepository}
\alias{CreateCustomModelsVersionsFromRepository}
\title{CreateCustomModelsVersionsFromRepository}
\usage{
CreateCustomModelsVersionsFromRepository(
  requiredMetadata,
  ref,
  baseEnvironmentId,
  customModelId,
  repositoryId,
  requiredMetadataValues,
  sourcePath,
  isMajorUpdate = TRUE,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Create custom model version from remote repository.
}
\details{
Create a new custom model version with only files added from the specified remote repository.

This method invokes \verb{POST /customModels/\{customModelId\}/versions/fromRepository/} in the DataRobot Public API.
}
\seealso{
The method CustomModelsVersionsFromRepositoryCreate in \link[datarobot.apicore:ModelsApi]{datarobot.apicore::ModelsApi}, which invokes the same underlying API endpoint.
}
