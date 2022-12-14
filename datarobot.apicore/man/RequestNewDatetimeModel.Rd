% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{RequestNewDatetimeModel}
\alias{RequestNewDatetimeModel}
\title{RequestNewDatetimeModel}
\usage{
RequestNewDatetimeModel(
  project,
  blueprint,
  featurelist = NULL,
  trainingRowCount = NULL,
  trainingDuration = NULL,
  timeWindowSamplePct = NULL,
  monotonicIncreasingFeaturelistId = NULL,
  monotonicDecreasingFeaturelistId = NULL,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Train a new datetime model.
}
\details{
Train a new datetime model.  All durations and datetimes should be specified in accordance with the \verb{timestamp and duration formatting rules<time_format>}.

This method invokes \verb{POST /projects/\{projectId\}/datetimeModels/} in the DataRobot Public API.
}
\seealso{
The method ProjectsDatetimeModelsCreate in \link{ModelsApi}, which invokes the same underlying API endpoint.
}
