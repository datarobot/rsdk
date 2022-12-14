% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictions_apiWrapper.R
\name{GetPredictionDataset}
\alias{GetPredictionDataset}
\title{GetPredictionDataset}
\usage{
GetPredictionDataset(project, datasetId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Get the metadata of a specific dataset. This only works for datasets uploaded to an existing project for prediction.
}
\details{
Get the metadata of a specific dataset. This only works for datasets uploaded to an existing project for prediction.

This method invokes \verb{GET /projects/\{projectId\}/predictionDatasets/\{datasetId\}/} in the DataRobot Public API.
}
\seealso{
The method ProjectsPredictionDatasetsRetrieve in \link{PredictionsApi}, which invokes the same underlying API endpoint.
}
