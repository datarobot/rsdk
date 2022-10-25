# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
IsDateTimePartition <- function(partition) {
  identical(partition$cvMethod, cvMethods$DATETIME)
}

IsMultiSeriesPartition <- function(partition) {
  "multiseriesIdColumns" %in% names(partition) && !is.null(partition$multiseriesIdColumns)
}

IsCrossSeriesGroupByPartition <- function(partition) {
  "crossSeriesGroupByColumns" %in% names(partition) &&
    !is.null(partition$crossSeriesGroupByColumns)
}

#' Start a project, set the target, and run autopilot.
#'
#' This function is a convenient shorthand to start a project and set the target.
#' See \code{SetupProject} and \code{SetTarget}.
#'
#' @inheritParams SetTarget
#' @inheritParams SetupProject
#' @inheritParams SetupProjectFromDataSource
#' @inheritParams WaitForAutopilot
#' @inheritParams UpdateProject
#' @param wait logical. If \code{TRUE}, invokes \code{WaitForAutopilot} to block execution until
#'   the autopilot is complete.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' StartProject(iris,
#'   projectName = "iris",
#'   target = "Species",
#'   targetType = TargetType$Multiclass
#' )
#' }
#' @export
StartProject <- function(dataSource, projectName = NULL, target, metric = NULL, weights = NULL,
                         partition = NULL, mode = NULL, seed = NULL, targetType = NULL,
                         positiveClass = NULL, blueprintThreshold = NULL,
                         responseCap = NULL, featurelistId = NULL,
                         smartDownsampled = NULL, majorityDownsamplingRate = NULL,
                         scaleoutModelingMode = NULL, accuracyOptimizedBlueprints = NULL,
                         offset = NULL, exposure = NULL, eventsCount = NULL,
                         monotonicIncreasingFeaturelistId = NULL,
                         monotonicDecreasingFeaturelistId = NULL,
                         onlyIncludeMonotonicBlueprints = FALSE, workerCount = NULL,
                         wait = FALSE, checkInterval = 20, timeout = NULL,
                         username = NULL, password = NULL, verbosity = 1, maxWait = 600) {
  if (is.null(projectName)) {
    projectName <- deparse(substitute(dataSource))
  }
  if (is(dataSource, "dataRobotDataSource")) {
    if (is.null(username) || is.null(password)) {
      stop("Username and password must be defined to start a project with a database.")
    }
    project <- SetupProjectFromDataSource(
      dataSourceId = dataSource,
      username = username,
      password = password,
      projectName = projectName,
      maxWait = maxWait
    )
  } else {
    project <- SetupProject(dataSource = dataSource, projectName = projectName, maxWait = maxWait)
  }
  SetTarget(project,
    target = target, metric = metric, weights = weights,
    partition = partition, mode = mode, seed = seed, targetType = targetType,
    positiveClass = positiveClass, blueprintThreshold = blueprintThreshold,
    responseCap = responseCap, featurelistId = featurelistId,
    smartDownsampled = smartDownsampled,
    majorityDownsamplingRate = majorityDownsamplingRate,
    scaleoutModelingMode = scaleoutModelingMode,
    accuracyOptimizedBlueprints = accuracyOptimizedBlueprints,
    offset = offset, exposure = exposure, eventsCount = eventsCount,
    monotonicIncreasingFeaturelistId = monotonicIncreasingFeaturelistId,
    monotonicDecreasingFeaturelistId = monotonicDecreasingFeaturelistId,
    onlyIncludeMonotonicBlueprints = onlyIncludeMonotonicBlueprints,
    maxWait = maxWait
  )
  if (!is.null(workerCount)) {
    UpdateProject(project, workerCount = workerCount)
  }
  if (isTRUE(wait)) {
    WaitForAutopilot(project,
      checkInterval = checkInterval,
      timeout = timeout, verbosity = verbosity
    )
  }
  project
}
