# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Retrieve a new or updated blender model defined by modelJobId
#'
#' The function RequestBlender initiates the creation of new blender models in a
#' DataRobot project.
#'
#' It submits requests to the DataRobot modeling
#' engine and returns an integer-valued modelJobId. The
#' GetBlenderModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotBlenderModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete. Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' @inheritParams DeleteProject
#' @param modelJobId integer. The integer returned by RequestBlender.
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to
#'   complete.
#' @inherit GetBlenderModel return
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelsToBlend <- c("5996f820af07fc605e81ead4", "59a5ce3301e9f0296721c64c")
#' blendJobId <- RequestBlender(projectId, modelsToBlend, "GLM")
#' GetBlenderModelFromJobId(projectId, blendJobId)
#' }
#' @export
GetBlenderModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Blender Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString,
    maxWait = maxWait,
    failureStatuses = JobFailureStatuses
  )
  modelId <- modelDetails$id
  returnModel <- GetBlenderModel(projectId, modelId)
  message("Blender Model ", modelId, " retrieved")
  return(as.dataRobotBlenderModel(returnModel))
}

as.dataRobotBlenderModel <- function(inList) {
  outList <- inList
  # rename to modelId
  idIndex <- which(names(outList) == "id")
  names(outList)[idIndex] <- "modelId"
  class(outList) <- "dataRobotBlenderModel"
  outList
}
