% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{GetFrozenModel}
\alias{GetFrozenModel}
\title{GetFrozenModel}
\usage{
GetFrozenModel(project, modelId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Look up a particular frozen model.
}
\details{
Look up a particular frozen model. If model with given ID exists but it's not frozen, returns 404 Not Found.

This method invokes \verb{GET /projects/\{projectId\}/frozenModels/\{modelId\}/} in the DataRobot Public API.
}
\seealso{
The method ProjectsFrozenModelsRetrieve in \link{ModelsApi}, which invokes the same underlying API endpoint.
}
