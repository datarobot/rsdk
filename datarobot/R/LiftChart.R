# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' Retrieve lift chart data for a model for a data partition (see DataPartition)
#'
#' @param model dataRobotModel. A DataRobot model object like that returned by \code{GetModel}.
#' @param source character. The data partition for which data would be returned. Default is
#'   \code{DataPartition$VALIDATION}. See \code{DataPartition} for details.
#' @param fallbackToParentInsights logical. If TRUE, this will return the lift chart data for the
#'   model's parent if the lift chart is not available for the model and the model has a parent
#'   model.
#' @return data.frame with the following components:
#' \itemize{
#'   \item binWeight. Numeric: weight of the bin.  For weighted projects, the sum of the weights of
#'     all rows in the bin; otherwise, the number of rows in the bin.
#'   \item actual. Numeric: sum of actual target values in bin.
#'   \item predicted. Numeric: sum of predicted target values in bin.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' GetLiftChart(model, source = DataPartition$VALIDATION)
#' }
#' @export
GetLiftChart <- function(model, source = DataPartition$VALIDATION,
                         fallbackToParentInsights = FALSE) {
  response <- GetGeneralizedInsight("liftChart", model, source = source,
                                    fallbackToParentInsights = fallbackToParentInsights)
  return(response$bins)
}

#' Retrieve lift chart data for a model for all available data partitions (see DataPartition)
#'
#' @inheritParams GetLiftChart
#' @inherit GetLiftChart return
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' ListLiftCharts(model)
#' }
#' @export
ListLiftCharts <- function(model, fallbackToParentInsights = FALSE) {
  response <- GetGeneralizedInsight("liftChart", model,
    source = NULL,
    fallbackToParentInsights = fallbackToParentInsights
  )
  names(response$charts$bins) <- response$charts$source
  return(as.list(response$charts$bins))
}
