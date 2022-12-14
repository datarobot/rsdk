% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{GetPrimeModel}
\alias{GetPrimeModel}
\title{GetPrimeModel}
\usage{
GetPrimeModel(project, modelId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve a Prime model details
}
\details{
Retrieve a Prime model details

This method invokes \verb{GET /projects/\{projectId\}/primeModels/\{modelId\}/} in the DataRobot Public API.
}
\seealso{
The method ProjectsPrimeModelsRetrieve in \link{ModelsApi}, which invokes the same underlying API endpoint.
}
