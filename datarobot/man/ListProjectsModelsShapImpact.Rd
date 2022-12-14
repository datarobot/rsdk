% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{ListProjectsModelsShapImpact}
\alias{ListProjectsModelsShapImpact}
\title{ListProjectsModelsShapImpact}
\usage{
ListProjectsModelsShapImpact(modelId, projectId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve Feature Impact for a model.
}
\details{
Retrieve Feature Impact for a model. Shap impact is computed by calculating the shap values on a sample of training data and then taking the mean absolute value for each column. The larger value of impact indicate more important feature.

This method invokes \verb{GET /projects/\{projectId\}/models/\{modelId\}/shapImpact/} in the DataRobot Public API.
}
\seealso{
The method ProjectsModelsShapImpactList in \link[datarobot.apicore:InsightsApi]{datarobot.apicore::InsightsApi}, which invokes the same underlying API endpoint.
}
