% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{ListProjectsRecommendedModelsRecommendedModel}
\alias{ListProjectsRecommendedModelsRecommendedModel}
\title{ListProjectsRecommendedModelsRecommendedModel}
\usage{
ListProjectsRecommendedModelsRecommendedModel(projectId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Get recommended model
}
\details{
This route returns the simplest recommended model available. To see all the available recommended models, use :\url{http:get}:/api/v2/projects/(projectId)/recommendedModels/

This method invokes \verb{GET /projects/\{projectId\}/recommendedModels/recommendedModel/} in the DataRobot Public API.
}
\seealso{
The method ProjectsRecommendedModelsRecommendedModelList in \link[datarobot.apicore:ModelsApi]{datarobot.apicore::ModelsApi}, which invokes the same underlying API endpoint.
}
