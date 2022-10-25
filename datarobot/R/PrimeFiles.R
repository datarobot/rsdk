# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Retrieve a specific Prime file from a DataRobot project for corresponding jobId
#'
#' @inheritParams DeleteProject
#' @param jobId numeric. Unique integer identifier (return for example by \code{RequestPrimeModel})
#' @param maxWait numeric. maximum time to wait (in sec) before job completed.
#' @return List with following elements:
#' \describe{
#'   \item{language}{Character string. Code programming language}
#'   \item{isValid}{logical flag indicating if code passed validation}
#'   \item{rulesetId}{Integer identifier for the ruleset}
#'   \item{parentModelId}{Unique alphanumeric identifier for the parent model}
#'   \item{projectId}{Unique alphanumeric identifier for the project}
#'   \item{id}{Unique alphanumeric identifier for the Prime file}
#'   \item{modelId}{Unique alphanumeric identifier for the model}
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' initialJobs <- ListModelJobs(project)
#' job <- initialJobs[[1]]
#' modelJobId <- job$modelJobId
#' GetPrimeFileFromJobId(projectId, modelJobId)
#' }
#' @export
GetPrimeFileFromJobId <- function(project, jobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  response <- WaitForAsyncReturn(routeString, maxWait,
    failureStatuses = JobFailureStatuses
  )
  GetPrimeFile(project, response$id)
}
