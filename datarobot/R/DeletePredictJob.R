# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' @name DeletePredictJob
#' @details Function to delete one predict job from the DataRobot queue
#'
#' This function deletes the predict job specified by predictJobId from
#' the DataRobot queue.
#'
#' @inheritParams DeleteProject
#' @inheritParams GetPredictions
#' @param predictJobId integer. The integer ID \code{predictionJobId}
#'   that is created by the call to \code{RequestPredictions}.
#' @return Logical TRUE and displays a message to the user if the delete
#' request was successful; otherwise, execution halts and an error message
#' is displayed.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' initialJobs <- GetPredictJobs(project)
#' job <- initialJobs[[1]]
#' predictJobId <- job$predictJobId
#' DeletePredictJob(projectId, predictJobId)
#' }
#' @export
#' @include predictions_apiWrapper.R
DeletePredictJob
