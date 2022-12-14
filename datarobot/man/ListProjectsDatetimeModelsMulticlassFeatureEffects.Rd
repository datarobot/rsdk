% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{ListProjectsDatetimeModelsMulticlassFeatureEffects}
\alias{ListProjectsDatetimeModelsMulticlassFeatureEffects}
\title{ListProjectsDatetimeModelsMulticlassFeatureEffects}
\usage{
ListProjectsDatetimeModelsMulticlassFeatureEffects(
  backtestIndex,
  modelId,
  class,
  projectId,
  includeIcePlots,
  offset = 0,
  limit = 100,
  source = "training",
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve feature effects for each class in a multiclass datetime model.
}
\details{
Retrieve feature effects for each class in a multiclass datetime model.

This method invokes \verb{GET /projects/\{projectId\}/datetimeModels/\{modelId\}/multiclassFeatureEffects/} in the DataRobot Public API.
}
\seealso{
The method ProjectsDatetimeModelsMulticlassFeatureEffectsList in \link[datarobot.apicore:InsightsApi]{datarobot.apicore::InsightsApi}, which invokes the same underlying API endpoint.
}
