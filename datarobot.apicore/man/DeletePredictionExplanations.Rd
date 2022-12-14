% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R,
%   R/predictions_apiWrapper.R
\name{DeletePredictionExplanations}
\alias{DeletePredictionExplanations}
\title{DeletePredictionExplanations}
\usage{
DeletePredictionExplanations(project, predictionExplanationId, ...)

DeletePredictionExplanations(project, predictionExplanationId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Delete saved Prediction Explanations.

Delete saved Prediction Explanations.
}
\details{
Delete saved Prediction Explanations. Deletes both the actual prediction explanations and the corresponding PredictionExplanationsRecord.

This method invokes \verb{DELETE /projects/\{projectId\}/predictionExplanationsRecords/\{predictionExplanationsId\}/} in the DataRobot Public API.

Delete saved Prediction Explanations. Deletes both the actual prediction explanations and the corresponding PredictionExplanationsRecord.

This method invokes \verb{DELETE /projects/\{projectId\}/predictionExplanationsRecords/\{predictionExplanationsId\}/} in the DataRobot Public API.
}
\seealso{
The method ProjectsPredictionExplanationsRecordsDelete in \link{InsightsApi}, which invokes the same underlying API endpoint.

The method ProjectsPredictionExplanationsRecordsDelete in \link{PredictionsApi}, which invokes the same underlying API endpoint.
}
