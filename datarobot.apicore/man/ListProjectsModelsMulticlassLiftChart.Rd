% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{ListProjectsModelsMulticlassLiftChart}
\alias{ListProjectsModelsMulticlassLiftChart}
\title{ListProjectsModelsMulticlassLiftChart}
\usage{
ListProjectsModelsMulticlassLiftChart(modelId, projectId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve all available lift charts for multiclass model.
}
\details{
Retrieve all available lift charts for multiclass model.

This method invokes \verb{GET /projects/\{projectId\}/models/\{modelId\}/multiclassLiftChart/} in the DataRobot Public API.
}
\seealso{
The method ProjectsModelsMulticlassLiftChartList in \link{InsightsApi}, which invokes the same underlying API endpoint.
}
