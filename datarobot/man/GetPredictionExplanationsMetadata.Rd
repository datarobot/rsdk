% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R,
%   R/predictions_apiWrapper.R, R/PredictionExplanations.R
\name{GetPredictionExplanationsMetadata}
\alias{GetPredictionExplanationsMetadata}
\title{GetPredictionExplanationsMetadata}
\usage{
GetPredictionExplanationsMetadata(
  project,
  predictionExplanationId,
  datasetId = NULL,
  finishTime = NULL,
  id = NULL,
  maxExplanations = NULL,
  modelId = NULL,
  numColumns = NULL,
  predictionExplanationsLocation = NULL,
  predictionThreshold = NULL,
  thresholdHigh = NULL,
  thresholdLow = NULL,
  ...
)

GetPredictionExplanationsMetadata(
  project,
  predictionExplanationId,
  datasetId = NULL,
  finishTime = NULL,
  id = NULL,
  maxExplanations = NULL,
  modelId = NULL,
  numColumns = NULL,
  predictionExplanationsLocation = NULL,
  predictionThreshold = NULL,
  thresholdHigh = NULL,
  thresholdLow = NULL,
  ...
)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{predictionExplanationId}{character. Id of the prediction explanations.}

\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\value{
A named list which contains prediction explanation metadata:
\itemize{
  \item id character. ID of the record and prediction explanations computation result.
  \item projectId character. ID of the project the model belongs to.
  \item modelId character. ID of the model prediction explanations initialization is for.
  \item datasetId character. ID of the prediction dataset prediction explanations were
    computed for.
  \item maxExplanations integer. Maximum number of prediction explanations to supply per row of
    the dataset.
  \item thresholdLow numeric. The low threshold, below which a prediction must score in order
    for prediction explanations to be computed for a row in the dataset.
  \item thresholdHigh numeric. The high threshold, above which a prediction must score in order
    for prediction explanations to be computed for a row in the dataset.
  \item numColumns integer. The number of columns prediction explanations were computed for.
  \item finishTime. Numeric timestamp referencing when computation for these prediction
    explanations finished.
  \item predictionExplanationsLocation character. Where to retrieve the prediction
    explanations.
}
}
\description{
Retrieve a PredictionExplanationsRecord object.

Retrieve a PredictionExplanationsRecord object.
}
\details{
Retrieve a PredictionExplanationsRecord object. A PredictionExplanationsRecord contains metadata about the computed prediction explanations and the location at which the PredictionExplanations can be retrieved.

This method invokes \verb{GET /projects/\{projectId\}/predictionExplanationsRecords/\{predictionExplanationsId\}/} in the DataRobot Public API.

Retrieve a PredictionExplanationsRecord object. A PredictionExplanationsRecord contains metadata about the computed prediction explanations and the location at which the PredictionExplanations can be retrieved.

This method invokes \verb{GET /projects/\{projectId\}/predictionExplanationsRecords/\{predictionExplanationsId\}/} in the DataRobot Public API.

Retrieve metadata for specified prediction explanations
}
\examples{
\dontrun{
projectId <- "59a5af20c80891534e3c2bde"
modelId <- "5996f820af07fc605e81ead4"
datasets <- ListPredictionDatasets(projectId)
dataset <- datasets[[1]]
datasetId <- dataset$id
model <- GetModel(projectId, modelId)
jobId <- RequestPredictionExplanations(model, datasetId)
predictionExplanationId <- GetPredictionExplanationsMetadataFromJobId(projectId, jobId)$id
GetPredictionExplanationsMetadata(projectId, predictionExplanationId)
}
}
\seealso{
The method ProjectsPredictionExplanationsRecordsRetrieve in \link[datarobot.apicore:InsightsApi]{datarobot.apicore::InsightsApi}, which invokes the same underlying API endpoint.

The method ProjectsPredictionExplanationsRecordsRetrieve in \link[datarobot.apicore:PredictionsApi]{datarobot.apicore::PredictionsApi}, which invokes the same underlying API endpoint.
}
