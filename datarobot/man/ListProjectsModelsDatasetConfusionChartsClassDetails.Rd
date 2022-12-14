% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{ListProjectsModelsDatasetConfusionChartsClassDetails}
\alias{ListProjectsModelsDatasetConfusionChartsClassDetails}
\title{ListProjectsModelsDatasetConfusionChartsClassDetails}
\usage{
ListProjectsModelsDatasetConfusionChartsClassDetails(
  modelId,
  datasetId,
  className,
  projectId,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Calculate and sends frequency of class in distributed among other classes for actual and predicted data.
}
\details{
Calculate and sends frequency of class in distributed among other classes for actual and predicted data. A confusion chart class details for given class gives stats of misclassification done by model for given class for actual and predicted data. Available for multiclass projects only.

This method invokes \verb{GET /projects/\{projectId\}/models/\{modelId\}/datasetConfusionCharts/\{datasetId\}/classDetails/} in the DataRobot Public API.
}
\seealso{
The method ProjectsModelsDatasetConfusionChartsClassDetailsList in \link[datarobot.apicore:InsightsApi]{datarobot.apicore::InsightsApi}, which invokes the same underlying API endpoint.
}
