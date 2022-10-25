# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' Request creation of a transferable model
#'
#' Requests generation of an transferable model file for use in an on-premise
#' DataRobot standalone prediction environment. This function can only be used
#' if model export is enabled, and will only be useful if you have an on-premise
#' environment in which to import it.
#'
#' This function does not download the exported file. Use
#' `DownloadTransferableModel` for that.
#' @inheritParams DeleteProject
#' @param modelId numeric. Unique alphanumeric identifier for the model of interest.
#' @param predictionIntervalSize integer. Optional. Added in 2.19. For supervised
#'   time series projects, this is the desired prediction interval size for the
#'   exported model. A prediction interval is the range of values DataRobot expects
#'   actual values of the target to fall within 0 to 100 (inclusive).
#' @return jobId
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' jobId <- RequestTransferableModel(
#'   projectId,
#'   modelId,
#'   50
#' )
#' WaitForJobToComplete(projectId, jobId)
#' file <- file.path(tempdir(), "model.drmodel")
#' DownloadTransferableModel(projectObject, modelId, file)
#' }
#' @family Transferable Model functions
#' @md
#' @export
RequestTransferableModel <- function(project, modelId, predictionIntervalSize = NULL) {
  projectId <- ValidateProject(project)
  routeString <- "modelExports"
  body <- list(
    projectId = projectId,
    modelId = modelId
  )
  body$percentile <- as.integer(predictionIntervalSize)
  postResponse <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  routeString <- UrlJoin("projects", projectId, "jobs", JobIdFromResponse(postResponse))
  jobsResponse <- DataRobotGET(routeString, simplifyDataFrame = FALSE)
  return(jobsResponse$id)
}
