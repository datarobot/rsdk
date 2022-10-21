#' Retrieve information about all DataRobot Prime models for a DataRobot project
#'
#' This function requests the DataRobot Prime models information for the DataRobot
#' project specified by the project argument, described under Arguments.
#'
#' The function returns data.frame containing information about each DataRobot Prime model in a
#' project (one row per Prime model)
#'
#' @inheritParams DeleteProject
#' @return data.frame (classed as \code{dataRobotPrimeModels}) containing
#'   information about each DataRobot Prime model in a project (one row per
#'   Prime model).
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListPrimeModels(projectId)
#' }
#' @export
ListPrimeModels <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeModels")
  primeInfo <- DataRobotGET(routeString)
  primeInfo <- GetServerDataInRows(primeInfo)
  as.dataRobotPrimeModels(primeInfo)
}


#' Retrieve information about specified DataRobot Prime model.
#'
#' This function requests the DataRobot Prime model information for the DataRobot
#' project specified by the project argument, and modelId.
#'
#' The function returns list containing information about specified DataRobot Prime model.
#'
#' @inheritParams GetModel
#' @return list (classed as \code{dataRobotPrimeModel}) containing information
#'   about specified DataRobot Prime model.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetPrimeModel(projectId, modelId)
#' }
#' @export
GetPrimeModel <- function(project, modelId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeModels", modelId)
  primeInfo <- DataRobotGET(routeString)
  as.dataRobotPrimeModel(primeInfo)
}


#' Retrieve information about specified DataRobot Prime model using corresponding jobId.
#'
#' @inheritParams DeleteProject
#' @param jobId Unique integer identifier (return for example by RequestPrimeModel)
#' @param maxWait maximum time to wait (in sec) before job completed
#' @inherit GetPrimeModel return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- ListModelJobs(project)
#'   job <- initialJobs[[1]]
#'   modelJobId <- job$modelJobId
#'   GetPrimeModelFromJobId(projectId, modelJobId)
#' }
#' @export
GetPrimeModelFromJobId <- function(project, jobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  response <- WaitForAsyncReturn(routeString, maxWait,
                                 failureStatuses = JobFailureStatuses)
  GetPrimeModel(project, response$id)
}


ApplyPrimeModelSchema <- function(inList) {
  elements <- c("featurelistId",
                "processes",
                "featurelistName",
                "modelType",
                "projectId",
                "samplePct",
                "trainingRowCount",
                "modelCategory",
                "metrics",
                "score",
                "parentModelId",
                "ruleCount",
                "isFrozen",
                "blueprintId",
                "rulesetId",
                "id")
  ApplySchema(inList, elements, "metrics")
}

as.dataRobotPrimeModels <- function(inList) {
  outList <- ApplyPrimeModelSchema(inList)
  class(outList) <- c("dataRobotPrimeModels", "data.frame")
  outList
}

as.dataRobotPrimeModel <- function(inList) {
  outList <- ApplyPrimeModelSchema(inList)
  class(outList) <- c("dataRobotPrimeModel")
  outList
}
