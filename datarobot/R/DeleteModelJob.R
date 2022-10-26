# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' @name DeleteModelJob
#' @details Delete a model job from the modeling queue
#'
#' This function deletes the modeling job specified by modelJobId from
#' the DataRobot modeling queue.
#'
#' @param project character. Either (1) a character string giving the unique alphanumeric
#' identifier for the project, or (2) a list containing the element projectId
#' with this identifier.
#' @param modelJobId integer. Identifier for the modeling job to be
#' deleted; can be obtained from the results returned by the function
#' ListModelJobs.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' initialJobs <- ListModelJobs(project)
#' job <- initialJobs[[1]]
#' modelJobId <- job$modelJobId
#' DeleteModelJob(projectId, modelJobId)
#' }
#' @export
#' @include models_apiWrapper.R
DeleteModelJob
