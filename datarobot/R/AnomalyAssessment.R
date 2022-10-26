# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
as.dataRobotAnomalyAssessmentRecord <- function(inList) {
  outList <- inList
  if (!is.null(outList$startDate)) {
    outList$startDate <- ParseRFC3339Timestamp(outList$startDate)
  }
  if (!is.null(outList$endDate)) {
    outList$endDate <- ParseRFC3339Timestamp(outList$endDate)
  }
  class(outList) <- "dataRobotAnomalyAssessmentRecord"
  return(outList)
}


as.dataRobotShapFeatureContribution <- function(inList) {
  outList <- inList
  class(outList) <- "dataRobotShapFeatureContribution"
  return(outList)
}

as.dataRobotPredictionWithExplanationsRow <- function(inList) {
  outList <- inList
  outList$timestamp <- ParseRFC3339Timestamp(outList$timestamp)
  if (!is.null(outList$shapExplanation)) {
    outList$shapExplanation <- lapply(outList$shapExplanation, as.dataRobotShapFeatureContribution)
  }
  class(outList) <- "dataRobotPredictionWithExplanationsRow"
  return(outList)
}


as.dataRobotAnomalyAssessmentExplanations <- function(inList) {
  outList <- inList
  outList$startDate <- ParseRFC3339Timestamp(outList$startDate)
  outList$endDate <- ParseRFC3339Timestamp(outList$endDate)
  outList$data <- lapply(outList$data, as.dataRobotPredictionWithExplanationsRow)
  class(outList) <- "dataRobotAnomalyAssessmentExplanations"
  return(outList)
}


as.dataRobotPreviewBin <- function(inList) {
  outList <- inList
  if (is.null(outList$avgPredicted)) {
    outList$avgPredicted <- as.numeric(NA)
  }
  if (is.null(outList$maxPredicted)) {
    outList$maxPredicted <- as.numeric(NA)
  }
  outList$startDate <- ParseRFC3339Timestamp(outList$startDate)
  outList$endDate <- ParseRFC3339Timestamp(outList$endDate)
  class(outList) <- "dataRobotPreviewBin"
  return(outList)
}


as.dataRobotAnomalyAssessmentPredictionsPreview <- function(inList) {
  outList <- inList
  outList$startDate <- ParseRFC3339Timestamp(outList$startDate)
  outList$endDate <- ParseRFC3339Timestamp(outList$endDate)
  outList$previewBins <- lapply(outList$previewBins, as.dataRobotPreviewBin)
  class(outList) <- "dataRobotAnomalyAssessmentPredictionsPreview"
  return(outList)
}

#' @name InitializeAnomalyAssessment
#' @details Request anomaly assessment insight computation on the specified subset.
#'
#' @param projectId character. The ID of the project to compute insight for.
#' @param modelId character. The ID of the model to compute insight for.
#' @param backtest integer or "holdout". The backtest to compute insight for.
#' @param source "training" or "validation". The source to compute insight for.
#' @param seriesId character. Optional. The series id to compute insight for.
#'   Required for multiseries projects.

#' @return An object with anomaly assessment metadata:
#' \itemize{
#'   \item recordId. character. The ID of the record.
#'   \item projectId. character. The project ID of the record.
#'   \item modelId. character. The model ID of the record.
#'   \item backtest. character. The backtest of the record.
#'   \item source. character. The source of the record.
#'   \item seriesId. character. the series ID of the record.
#'   \item status. character. The status of the insight.
#'   \item statusDetails. character. The explanation of the status.
#'   \item startDate. POSIXct. Timestamp of the first prediction in the subset. Will be NULL if
#'     status is not `completed`.
#'   \item endDate. POSIXct. Timestamp of the last prediction in the subset. Will be NULL
#'     if status is not `completed`.
#'   \item predictionThreshold. numeric. The threshold, all rows with anomaly scores greater or
#'     equal to it have shap explanations computed. Will be NULL if status is not `completed`.
#'   \item previewLocation. character. URL to retrieve predictions preview for the subset.
#'     Will be NULL if status is not `completed`.
#'   \item latestExplanationsLocation. character. the URL to retrieve the latest predictions with
#'     the shap explanations. Will be NULL if status is not `completed`.
#'   \item deleteLocation. character. the URL to delete anomaly assessment record and
#'     relevant insight data.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "59a5af20c80891534e3c2bdd"
#' record <- InitializeAnomalyAssessment(projectId, modelId,
#'   backtest = 0, source = "validation",
#'   seriesId = "Baltimore"
#' )
#' }
#' @family Anomaly Assessment functions
#' @md
#' @export
#' @include insights_apiWrapper.R
InitializeAnomalyAssessment

#' @name ListAnomalyAssessmentRecords
#' @details Retrieve anomaly assessment records.
#'
#' @param projectId character. The ID of the project.
#' @param modelId character. The ID of the model.
#' @param seriesId character. Optional. Can be specified for multiseries projects.
#' The series id to filter records by.
#' @param backtest integer or "holdout". Optional.  The backtest to filter records by.
#' @param source "training" or "validation". Optional. The source of the data to filter records by.
#' @param offset integer. Optional. Default is 0. This many results will be skipped.
#' @param limit integer, greater than zero. Optional. Defaults to 100. At most this many results
#'   are returned. The default may change without notice.

#' @return A list of objects with anomaly assessment metadata:
#' \itemize{
#'   \item recordId. character. The ID of the record.
#'   \item projectId. character. The project ID of the record.
#'   \item modelId. character. The model ID of the record.
#'   \item backtest. character. The backtest of the record.
#'   \item source. character. The source of the record.
#'   \item seriesId. character. the series ID of the record.
#'   \item status. character. The status of the insight.
#'   \item statusDetails. character. The explanation of the status.
#'   \item startDate. POSIXct. Timestamp of the first prediction in the subset. Will be NULL if
#'     status is not `completed`.
#'   \item endDate. POSIXct. Timestamp of the last prediction in the subset. Will be NULL
#'     if status is not `completed`.
#'   \item predictionThreshold. numeric. The threshold, all rows with anomaly scores greater or
#'     equal to it have shap explanations computed. Will be NULL if status is not `completed`.
#'   \item previewLocation. character. URL to retrieve predictions preview for the subset.
#'     Will be NULL if status is not `completed`.
#'   \item latestExplanationsLocation. character. the URL to retrieve the latest predictions with
#'     the shap explanations. Will be NULL if status is not `completed`.
#'   \item deleteLocation. character. the URL to delete anomaly assessment record and
#'     relevant insight data.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "59a5af20c80891534e3c2bdd"
#' records <- ListAnomalyAssessmentRecords(projectId, modelId, backtest = 0, seriesId = "Baltimore")
#' }
#' @family Anomaly Assessment functions
#' @md
#' @export
#' @include insights_apiWrapper.R
ListAnomalyAssessmentRecords

#' @name GetAnomalyAssessmentExplanations
#' @details Retrieve anomaly assessment explanations.
#'
#' Explanations contain predictions along with shap explanations for the most anomalous records
#' in the specified date range/for defined number of points.
#' Two out of three parameters: startDate, endDate or pointsCount must be specified.
#'
#' @param projectId character. The ID of the project.
#' @param recordId character. The ID of the anomaly assessment record.
#' @param startDate POSIXct. Optional. The start of the date range to get explanations in.
#' @param endDate POSIXct. Optional. The end of the date range to get explanations in.
#' @param pointsCount integer. Optional. The number of the rows to return.

#' @return The anomaly assessment explanations:
#' \itemize{
#'   \item recordId. character. The ID of the record.
#'   \item projectId. character. The project ID of the record.
#'   \item modelId. character. The model ID of the record.
#'   \item backtest. character. The backtest of the record.
#'   \item source. character. The source of the record.
#'   \item seriesId. character. the series ID of the record.
#'   \item startDate. POSIXct. First timestamp in the response. Will be NULL if there is no
#'     data in the specified range.
#'   \item endDate. POSIXct. Last timestamp in the response. Will be NULL if there is no
#'     data in the specified range.
#'   \item shapBaseValue. numeric. Shap base value.
#'   \item count. integer.  The number of points in the ``data``.
#'   \item data. list. A list of DataPoint objects in the specified date range containing:
#'   \itemize{
#'     \item shapExplanation. NULL or an array of up to 10 ShapleyFeatureContribution objects.
#'       Only rows with the highest anomaly scores have Shapley explanations calculated.
#'     \item timestamp POSIXct. Timestamp for the row.
#'     \item prediction numeric. The output of the model for this row.
#'      }
#'
#'     Each ShapleyFeatureContribution contains:
#'   \itemize{
#'     \item featureValue. character. The feature value for this row.
#'        First 50 characters are returned.
#'     \item strength numeric. The shap value for this feature and row.
#'     \item feature character. The feature name.
#'      }
#'
#'
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' recordId <- "59a5af20c80891534e3c2bdb"
#' explanations <- GetAnomalyAssessmentExplanations(projectId, recordId,
#'   pointsCount = 100,
#'   startDate = as.Date("2021-01-01")
#' )
#' }
#' @family Anomaly Assessment functions
#' @md
#' @export
#' @include insights_apiWrapper.R
GetAnomalyAssessmentExplanations


#' @name GetAnomalyAssessmentPredictionsPreview
#' @details Retrieve anomaly assessment predictions preview.
#'
#' Aggregated predictions over time for the corresponding anomaly assessment record.
#' Intended to find the bins with highest anomaly scores.
#'
#' @param projectId character. The ID of the project.
#' @param recordId character. The ID of the anomaly assessment record.
#'
#' @return The anomaly assessment predictions preview:
#' \itemize{
#'   \item recordId. character. The ID of the record.
#'   \item projectId. character. The project ID of the record.
#'   \item modelId. character. The model ID of the record.
#'   \item backtest. character. The backtest of the record.
#'   \item source. character. The source of the record.
#'   \item seriesId. character. the series ID of the record.
#'   \item startDate. POSIXct. Timestamp of the first prediction in the subset.
#'   \item endDate. POSIXct. Timestamp of the last prediction in the subset.
#'   \item previewBins. list. A list of PreviewBin objects in the specified date range.
#'     The aggregated predictions for the subset. Bins boundaries may differ from actual start/end
#'      dates because this is an aggregation. Each PreviewBin contains:
#'   \itemize{
#'     \item startDate. POSIXct. Datetime of the start of the bin.
#'     \item endDate. POSIXct. Datetime of the end of the bin.
#'     \item avgPredicted numeric. The average prediction of the model in the bin. NA if
#'       there are no entries in the bin.
#'     \item maxPredicted numeric. The maximum prediction of the model in the bin. NA if
#'        there are no entries in the bin.
#'     \item frequency integer. The number of the rows in the bin.
#'      }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' recordId <- "59a5af20c80891534e3c2bdb"
#' explanations <- GetAnomalyAssessmentPredictionsPreview(projectId, recordId)
#' }
#' @family Anomaly Assessment functions
#' @md
#' @export
#' @include insights_apiWrapper.R
GetAnomalyAssessmentPredictionsPreview


#' @name DeleteAnomalyAssessmentRecord
#' @details Delete anomaly assessment record.
#'
#' Record is deleted with preview and explanations.
#'
#' @param projectId character. The ID of the project.
#' @param recordId character. The ID of the anomaly assessment record.
#'
#' @return NULL
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' recordId <- "59a5af20c80891534e3c2bdb"
#' explanations <- DeleteAnomalyAssessmentRecord(projectId, recordId)
#' }
#' @family Anomaly Assessment functions
#' @md
#' @export
#' @include insights_apiWrapper.R
DeleteAnomalyAssessmentRecord
