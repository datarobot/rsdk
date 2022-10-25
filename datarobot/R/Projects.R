# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Retrieve a list of all DataRobot projects
#'
#' This function returns an S3 object of class projectSummaryList
#' that describes all (optionally filtered) DataRobot modeling projects available to the user.
#' This list may be converted into a dataframe with the as.data.frame
#' method for this class of S3 objects.
#'
#' @param filter list. Optional. A named list that can be used to specify various filters.
#'  Currently `projectName` is supported which will filter returned projects for projects with
#'  names containing the specified string.
#'
#' @return An S3 object of class 'projectSummaryList', consisting of the following elements:
#' \itemize{
#'   \item projectId. List of character strings giving the unique DataRobot identifier for each
#'     project.
#'   \item projectName. List of character strings giving the user-supplied project names.
#'   \item fileName. List of character strings giving the name of the modeling dataset for each
#'     project.
#'   \item stage. List of character strings specifying each project's Autopilot stage (e.g., 'aim'
#'     is necessary to set target). Use \code{ProjectStage} to get a list of options.
#'   \item autopilotMode. List of integers specifying the Autopilot mode (0 = fully automatic,
#'     1 = semi-automatic, 2 = manual).
#'   \item created. List of character strings giving the project creation time and date.
#'   \item target. List of character strings giving the name of the target variable for each
#'     project.
#'   \item metric. List of character strings identifying the fitting metric optimized for each
#'     project.
#'   \item partition. Dataframe with one row for each project and 12 columns specifying
#'     partitioning details.
#'   \item advancedOptions. Dataframe with one row for each project and 4 columns specifying values
#'     for advanced option parameters.
#'   \item positiveClass. Character string identifying the positive target class for binary
#'     classification projects.
#'   \item maxTrainPct. The maximum percentage of the project dataset that can be used without going
#'     into the validation data or being too large to submit any blueprint for training a project.
#'   \item maxTrainRows. The maximum number of rows that can be trained on without going into the
#'     validation data or being too large to submit any blueprint for training.
#'   \item scaleoutMaxTrainPct. The maximum percentage of the project dataset that can be used to
#'     successfully train a scaleout model without going into the validation data. May exceed
#'     \code{maxTrainPct}, in which case only scaleout models can be trained up to this point.
#'   \item scaleoutMaxTrainRows. The maximum number of rows that can be used to successfully
#'     train a scaleout model without going into the validation data. May exceed
#'     \code{maxTrainRows}, in which case only scaleout models can be trained up to this point.
#'   \item holdoutUnlocked. Logical flag indicating whether holdout subset results have been
#'     computed.
#'   \item targetType. Character string giving the type of modeling project (e.g., regression or
#'     binary classification).
#' }
#' @examples
#' \dontrun{
#' ListProjects()
#' ListProjects(filter = list("projectName" = "TimeSeries"))
#' }
#' @export
ListProjects <- function(filter = NULL) {
  routeString <- "projects/"
  params <- NULL
  if (!is.null(filter)) {
    if (!is.list(filter)) {
      stop("`filter` must be a list.")
    }
    if ("projectName" %in% names(filter)) {
      if (length(filter$projectName) != 1) {
        stop("`projectName` must be a character vector of length 1.")
      }
      params <- list("projectName" = filter$projectName)
    }
  }
  returnValue <- DataRobotGET(routeString, query = params)
  projectSummaryList(returnValue)
}


projectSummaryList <- function(projectSummaryData) {
  if (length(projectSummaryData) == 0) {
    emptyProjectSummaryList <- structure(
      list(
        projectId = character(0), projectName = character(0),
        fileName = character(0), stage = character(0), autopilotMode = logical(0),
        created = character(0), target = logical(0), metric = logical(0),
        partition = data.frame(
          datetimeCol = logical(0), cvMethod = logical(0),
          validationPct = logical(0), reps = logical(0),
          cvHoldoutLevel = logical(0), holdoutLevel = logical(0),
          userPartitionCol = logical(0), validationType = logical(0),
          trainingLevel = logical(0), partitionKeyCols = logical(0),
          holdoutPct = logical(0), validationLevel = logical(0)
        ),
        advancedOptions = data.frame(
          blueprintThreshold = logical(0),
          responseCap = logical(0), seed = logical(0),
          weights = logical(0)
        ),
        positiveClass = logical(0), maxTrainPct = logical(0), maxTrainRows = logical(0),
        scaleoutMaxTrainPct = logical(0), scaleoutMaxTrainRows = logical(0),
        holdoutUnlocked = logical(0),
        targetType = logical(0)
      ),
      class = "projectSummaryList"
    )
    return(emptyProjectSummaryList)
  } else {
    idIndex <- which(names(projectSummaryData) == "id")
    names(projectSummaryData)[[idIndex]] <- "projectId"
    projectSummaryData <- as.dataRobotProject(projectSummaryData)
    class(projectSummaryData) <- "projectSummaryList"
    return(projectSummaryData)
  }
}

as.dataRobotProject <- function(inProject) {
  outProject <- inProject
  class(outProject) <- "dataRobotProject"
  outProject
}
