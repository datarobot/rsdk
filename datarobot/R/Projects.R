#' Delete a specified element from the DataRobot project list
#'
#' This function deletes the project defined by project,
#' described under Arguments. This parameter may be obtained
#' in several ways, including: (1), as one of the projectId
#' elements of the list returned by ListProjects; (2), as
#' the S3 object returned by the GetProject function; or (3),
#' as the list returned by the SetupProject function.
#'
#' @param project character. Either (1) a character string giving the unique alphanumeric
#'   identifier for the project, or (2) a list containing the element projectId with this
#'   identifier.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   DeleteProject(projectId)
#' }
#' @export
DeleteProject <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId)
  response <- DataRobotDELETE(routeString)
  if (is.list(project)) {
    projectName <- project$projectName
  } else {
    projectName <- ""
  }
  message(paste("Project", projectName, "with projectId = ",
                projectId, "deleted"))
}


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
#' @param limit integer. Optional. At most this many results are returned, default: 1000
#' @param offset integer. Optional. This many results will be skipped, default: 0
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
#'   \item holdoutUnlocked. Logical flag indicating whether holdout subset results have been
#'     computed.
#'   \item targetType. Character string giving the type of modeling project (e.g., regression or
#'     binary classification).
#' }
#' @examples
#' \dontrun{
#'   ListProjects()
#'   ListProjects(filter = list("projectName" = "TimeSeries"))
#' }
#' @export
ListProjects <- function(filter = NULL, limit = 1000, offset = 0) {
  routeString <- "projects/"
  params <- list(offset = offset, limit = limit)
  if (!is.null(filter)) {
    if (!is.list(filter)) {
      stop("`filter` must be a list.")
    }
    if ("projectName" %in% names(filter)) {
      if (length(filter$projectName) != 1) {
        stop("`projectName` must be a character vector of length 1.")
      }
      params$projectName <- filter$projectName
    }
  }
  returnValue <- DataRobotGET(routeString, query = params)
  projectSummaryList(returnValue)
}


projectSummaryList <- function(projectSummaryData) {
  if (length(projectSummaryData) == 0) {
    emptyProjectSummaryList <- structure(list(projectId = character(0), projectName = character(0),
                 fileName = character(0), stage = character(0), autopilotMode = logical(0),
                 created = character(0), target = logical(0), metric = logical(0),
                 partition = data.frame(datetimeCol = logical(0), cvMethod = logical(0),
                                        validationPct = logical(0), reps = logical(0),
                                        cvHoldoutLevel = logical(0),  holdoutLevel = logical(0),
                                        userPartitionCol = logical(0),  validationType = logical(0),
                                        trainingLevel = logical(0), partitionKeyCols = logical(0),
                                        holdoutPct = logical(0), validationLevel = logical(0)),
                 advancedOptions = data.frame(blueprintThreshold = logical(0),
                                              responseCap = logical(0), seed = logical(0),
                                              weights = logical(0)),
                 positiveClass = logical(0), maxTrainPct = logical(0), maxTrainRows = logical(0),
                 holdoutUnlocked = logical(0),
                 targetType = logical(0)),
            class = "projectSummaryList")
    return(emptyProjectSummaryList)
  } else {
    idIndex <- which(names(projectSummaryData) == "id")
    names(projectSummaryData)[[idIndex]] <- "projectId"
    projectSummaryData <- as.dataRobotProject(projectSummaryData)
    class(projectSummaryData) <- "projectSummaryList"
    return(projectSummaryData)
  }
}


#' Retrieve details about a specified DataRobot modeling project
#'
#' Returns a list of details about the DataRobot modeling project
#' specified by project.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class 'dataRobotProject', consisting of the following elements:
#' \itemize{
#'   \item projectId. Character string giving the unique project identifier.
#'   \item projectName. Character string giving the name assigned to the project.
#'   \item fileName. Character string giving the name of the modeling dataset for the project.
#'   \item stage. Character string describing the stage of the DataRobot Autopilot.
#'   \item autopilotMode. Numeric: 0 for fully automatic mode; 1 for semi-automatic mode; 2 for
#'     manual mode.
#'   \item created. Character string representation of the project creation time and date.
#'   \item target. Name of the target variable from fileName.
#'   \item metric. Character string specifying the metric optimized by all project models.
#'   \item partition. A 7-element list describing the data partitioning for model fitting
#'     and cross validation.
#'   \item advancedOptions. A 4-element list with advanced option specifications.
#'   \item positiveClass. Character string: name of positive class for binary response models.
#'   \item maxTrainPct. The maximum percentage of the project dataset that can be used without going
#'     into the validation data or being too large to submit any blueprint for training a project.
#'   \item maxTrainRows. The maximum number of rows that can be trained on without going into the
#'     validation data or being too large to submit any blueprint for training.
#'   \item holdoutUnlocked. A logical flag indicating whether the holdout dataset has been used for
#'     model evaluation.
#'   \item targetType. Character string specifying the type of modeling problem (e.g., regression or
#'     binary classification).
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetProject(projectId)
#' }
#' @export
GetProject <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId)
  projectDetails <- DataRobotGET(routeString)
  idIndex <- which(names(projectDetails) == "id")
  names(projectDetails)[idIndex] <- "projectId"
  return(as.dataRobotProject(projectDetails))
}

as.dataRobotProject <- function(inProject) {
  outProject <- inProject
  class(outProject) <- "dataRobotProject"
  outProject
}

#' Request Autopilot status for a specified DataRobot project
#'
#' This function polls the DataRobot Autopilot for the status
#' of the project specified by the project parameter.
#'
#' @inheritParams DeleteProject
#' @return List with the following three components:
#' \describe{
#'   \item{autopilotDone}{Logical flag indicating whether the Autopilot has completed}
#'   \item{stage}{Character string specifying the Autopilot stage}
#'   \item{stageDescription}{Character string interpreting the Autopilot stage value}
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetProjectStatus(projectId)
#' }
#' @export
GetProjectStatus <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "status")
  autopilotStatus <- DataRobotGET(routeString)
  as.dataRobotProjectStatus(autopilotStatus)
}

as.dataRobotProjectStatus <- function(inList) {
  elements <- c("autopilotDone",
                "stageDescription",
                "stage")
  ApplySchema(inList, elements)
}


#' Update parameters for an existing project
#'
#' This function updates parameters for the project defined by project.
#'
#' @inheritParams DeleteProject
#' @param newProjectName character. Updated value for the projectName parameter
#'   associated with the project.
#' @param holdoutUnlocked logical. Either NULL (default) or TRUE. If TRUE, this function
#'   requests the DataRobot Autopilot to unlock the holdout data subset.
#' @param workerCount integer. The number of workers to run (default 2). Use \code{"max"} to set
#'   to the maximum number of workers available.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   UpdateProject(projectId, newProjectName = "cooler Project")
#'   UpdateProject(projectId, workerCount = 20)
#'   UpdateProject(projectId, holdoutUnlocked = TRUE)
#' }
#' @export
UpdateProject <- function(project, newProjectName = NULL, workerCount = NULL,
                          holdoutUnlocked = NULL) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId)
  bodyList <- list()
  if (identical(workerCount, "max")) { workerCount <- -1 }
  bodyList$workerCount <- workerCount
  bodyList$holdoutUnlocked <- holdoutUnlocked
  bodyList$projectName <- newProjectName
  if (all(unlist(Map(is.null, bodyList)))) {
    stop("No update data is provided")
  }
  body <- jsonlite::unbox(as.data.frame(bodyList))
  response <- DataRobotPATCH(routeString, body = body, encode = "json")
  message(paste("Project", projectId, "updated"))
}
