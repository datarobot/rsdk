# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

PredictJobRoute <- function(projectId, predictJobId) {
  return(UrlJoin("projects", projectId, "predictJobs", predictJobId))
}

as.dataRobotPredictJobStatus <- function(inList) {
  outList <- inList
  idIndex <- which(names(outList) == "id")
  names(outList)[idIndex] <- "predictJobId"
  return(outList)
}

#' @name GetPredictJobs
#' @details Function to list all prediction jobs in a project
#'
#' @param project character. Either (1) a character string giving the unique alphanumeric
#'   identifier for the project, or (2) a list containing the element projectId with this
#'   identifier.
#' @param status character. The status of the desired jobs: one of JobStatus$Queue,
#'   JobStatus$InProgress, orJobStatus$Error. If NULL (default), queued and inprogress jobs
#'   are returned.
#' @return Dataframe with one row for each prediction job in the queue,
#'   with the following columns:
#'   \describe{
#'     \item{status}{Prediction job status; one of JobStatus$Queue, JobStatus$InProgress, or
#'   JobStatus$Error}
#'     \item{predictJobId}{Character string specifying the job id}
#'     \item{modelId}{Character string specifying the model from which
#'     predictions have been requested}
#'     \item{projectId}{Character string specifying the project that contains the model}
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' GetPredictJobs(projectId)
#' }
#' @export
#' @include predictions_apiWrapper.R
GetPredictJobs

#' @name GetPredictJob
#' @details Request information about a predict job
#'
#' @inheritParams DeleteProject
#' @param predictJobId Character string specifying the job id
#' @return list with following elements:
#' \describe{
#'   \item{status}{Prediction job status; an element of JobStatus, e.g. JobStatus$Queue}
#'   \item{predictJobId}{Character string specifying the job id}
#'   \item{modelId}{Character string specifying the model from which
#'   predictions have been requested}
#'   \item{projectId}{Character string specifying the project that contains the model}
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' initialJobs <- GetPredictJobs(project)
#' job <- initialJobs[[1]]
#' predictJobId <- job$predictJobId
#' GetPredictJob(projectId, predictJobId)
#' }
#' @export
#' @include predictions_apiWrapper.R
GetPredictJob
