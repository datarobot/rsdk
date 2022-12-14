% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{ListPrimeModels}
\alias{ListPrimeModels}
\title{ListPrimeModels}
\usage{
ListPrimeModels(project, offset = 0, limit = 100, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
List all Prime models in a project
}
\details{
List all Prime models in a project

This method invokes \verb{GET /projects/\{projectId\}/primeModels/} in the DataRobot Public API.
}
\seealso{
The method ProjectsPrimeModelsList in \link{ModelsApi}, which invokes the same underlying API endpoint.
}
