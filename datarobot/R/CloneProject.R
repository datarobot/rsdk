# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' @name CloneProject
#' @details Clone a project
#'
#' This function clones a project, creating a fresh (postEDA1) copy that will
#' need a target and modeling options set.
#'
#' @param project dataRobotProject, or a character representing that project's ID.
#' @param newProjectName character. The name of the newly cloned project. If no
#'   name is given, the API will default to 'Copy of \code{project$projectName}'.
#' @param maxWait integer. The maximum time to wait for each of two steps: (1) The initial
#'   project creation request, and (2) data processing that occurs after receiving the response
#'   to this initial request.
#' @inherit as.dataRobotProjectShort return
#' @examples
#' \dontrun{
#' project < GetProject("5c1303269300d900016b41a7")
#' CloneProject(project, newProjectName = "Project Restart")
#' }
#' @export
#' @include projects_apiWrapper.R
CloneProject
