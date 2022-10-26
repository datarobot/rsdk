# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' @name PauseQueue
#' @details Pause the DataRobot modeling queue
#'
#' This function pauses the DataRobot modeling queue for a specified project
#'
#' @inheritParams DeleteProject
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' PauseQueue(projectId)
#' }
#' @export
#' @include projects_apiWrapper.R
PauseQueue
