% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{ListProjectsDatetimeModelsDatasetAccuracyOverTimePlotsPreview}
\alias{ListProjectsDatetimeModelsDatasetAccuracyOverTimePlotsPreview}
\title{ListProjectsDatetimeModelsDatasetAccuracyOverTimePlotsPreview}
\usage{
ListProjectsDatetimeModelsDatasetAccuracyOverTimePlotsPreview(
  modelId,
  datasetId,
  projectId,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve a preview of the Accuracy Over Time (AOT) chart for an external dataset.
}
\details{
Retrieve a preview of the Accuracy Over Time (AOT) chart for an external dataset. Datetimes are specified in accordance with \verb{timestamp and duration formatting rules <time_format>}.

This method invokes \verb{GET /projects/\{projectId\}/datetimeModels/\{modelId\}/datasetAccuracyOverTimePlots/\{datasetId\}/preview/} in the DataRobot Public API.
}
\seealso{
The method ProjectsDatetimeModelsDatasetAccuracyOverTimePlotsPreviewList in \link{InsightsApi}, which invokes the same underlying API endpoint.
}
