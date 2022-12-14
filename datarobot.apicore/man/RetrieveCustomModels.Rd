% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{RetrieveCustomModels}
\alias{RetrieveCustomModels}
\title{RetrieveCustomModels}
\usage{
RetrieveCustomModels(customModelId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Get custom model.
}
\details{
Retrieve metadata for a custom model.

This method invokes \verb{GET /customModels/\{customModelId\}/} in the DataRobot Public API.
}
\seealso{
The method CustomModelsRetrieve in \link{ModelsApi}, which invokes the same underlying API endpoint.
}
