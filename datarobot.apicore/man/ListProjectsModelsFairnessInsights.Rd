% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{ListProjectsModelsFairnessInsights}
\alias{ListProjectsModelsFairnessInsights}
\title{ListProjectsModelsFairnessInsights}
\usage{
ListProjectsModelsFairnessInsights(
  fairnessMetricsSet,
  modelId,
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
List calculated Per Class Bias insights.
}
\details{
Retrieve a list of Per Class Bias insights for the model.

This method invokes \verb{GET /projects/\{projectId\}/models/\{modelId\}/fairnessInsights/} in the DataRobot Public API.
}
\seealso{
The method ProjectsModelsFairnessInsightsList in \link{InsightsApi}, which invokes the same underlying API endpoint.
}
