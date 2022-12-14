% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{ListProjectsDatetimeModels}
\alias{ListProjectsDatetimeModels}
\title{ListProjectsDatetimeModels}
\usage{
ListProjectsDatetimeModels(
  bulkOperationId,
  projectId,
  offset = 0,
  limit = 100,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
List datetime partitioned project models
}
\details{
List all the models from a datetime partitioned project.

This method invokes \verb{GET /projects/\{projectId\}/datetimeModels/} in the DataRobot Public API.
}
\seealso{
The method ProjectsDatetimeModelsList in \link[datarobot.apicore:ModelsApi]{datarobot.apicore::ModelsApi}, which invokes the same underlying API endpoint.
}
