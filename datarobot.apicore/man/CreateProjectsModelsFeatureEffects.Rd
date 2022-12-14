% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{CreateProjectsModelsFeatureEffects}
\alias{CreateProjectsModelsFeatureEffects}
\title{CreateProjectsModelsFeatureEffects}
\usage{
CreateProjectsModelsFeatureEffects(modelId, rowCount, projectId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Add a request to the queue to calculate Feature Effects.
}
\details{
Add a request to the queue to calculate Feature Effects. If the job has been previously submitted, the request fails, returning the jobId of the previously submitted job. Use this jobId to check status of the previously submitted job.

This method invokes \verb{POST /projects/\{projectId\}/models/\{modelId\}/featureEffects/} in the DataRobot Public API.
}
\seealso{
The method ProjectsModelsFeatureEffectsCreate in \link{InsightsApi}, which invokes the same underlying API endpoint.
}
