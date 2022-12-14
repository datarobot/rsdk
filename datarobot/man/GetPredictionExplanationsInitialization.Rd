% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PredictionExplanations.R
\name{GetPredictionExplanationsInitialization}
\alias{GetPredictionExplanationsInitialization}
\title{Retrieve the prediction explanations initialization for a model.}
\usage{
GetPredictionExplanationsInitialization(model)
}
\arguments{
\item{model}{An S3 object of class dataRobotModel like that returned by
the function GetModel, or each element of the list returned by
the function ListModels.}
}
\value{
A named list which contains:
\itemize{
  \item projectId character. ID of the project the feature belongs to.
  \item modelId character. The unique alphanumeric model identifier.
  \item predictionExplanationsSample list. List with sample of prediction explanations.
    Each element of the list is information about prediction explanations for one data row.
    For more information see \code{GetPredictionExplanationsRows}.
  }
}
\description{
Prediction explanations initializations are a prerequisite for computing prediction
explanations, and include a sample what the computed prediction explanations for a
prediction dataset would look like.
}
\examples{
\dontrun{
projectId <- "59a5af20c80891534e3c2bde"
modelId <- "5996f820af07fc605e81ead4"
model <- GetModel(projectId, modelId)
GetPredictionExplanationsInitialization(model)
}
}
