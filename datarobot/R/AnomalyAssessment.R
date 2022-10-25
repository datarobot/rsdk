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
