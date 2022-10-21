#' List all downloadable code files from DataRobot Prime for the project
#'
#' Training a model using a ruleset is a necessary prerequisite for being able to download the code
#' for a ruleset.
#'
#' @inheritParams DeleteProject
#' @param parentModelId numeric. Optional. Filter for only those prime files approximating this
#'   parent model.
#' @param modelId numeric. Optional. Filter for only those prime files with code for this
#'   prime model.
#' @return List of lists. Each element of the list corresponds to one Prime file available to
#'   download. The elements of this list have the same format as the return value of GetPrimeFile.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListPrimeFiles(projectId)
#' }
#' @export
ListPrimeFiles <- function(project, parentModelId = NULL, modelId = NULL) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeFiles")
  query <- list()
  if (!is.null(parentModelId)) {
    query$parentModelId <- parentModelId
  }
  if (!is.null(modelId)) {
    query$modelId <- modelId
  }
  response <- DataRobotGET(routeString, query = query,
                           simplifyDataFrame = FALSE, encode = "json")
  response <- GetServerDataInRows(response)
  lapply(response, as.dataRobotPrimeFile)
}


#' Retrieve a specific Prime file from a DataRobot project
#'
#' This function returns information about specified Prime file from a specified project.
#'
#' @inheritParams DeleteProject
#' @param primeFileId numeric. Unique alphanumeric identifier for the primeFile
#' to be retrieved.
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
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   primeFiles <- ListPrimeFiles(projectId)
#'   primeFile <- primeFiles[[1]]
#'   primeFileId <- primeFile$id
#'   GetPrimeFile(projectId, primeFileId)
#' }
#' @export
GetPrimeFile <- function(project, primeFileId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeFiles", primeFileId)
  response <- DataRobotGET(routeString)
  as.dataRobotPrimeFile(response)
}


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
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- ListModelJobs(project)
#'   job <- initialJobs[[1]]
#'   modelJobId <- job$modelJobId
#'   GetPrimeFileFromJobId(projectId, modelJobId)
#' }
#' @export
GetPrimeFileFromJobId <- function(project, jobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  response <- WaitForAsyncReturn(routeString, maxWait,
                                 failureStatuses = JobFailureStatuses)
  GetPrimeFile(project, response$id)
}


as.dataRobotPrimeFile <- function(inList) {
  elements <- c("language",
                "isValid",
                "rulesetId",
                "parentModelId",
                "projectId",
                "id",
                "modelId")
  ApplySchema(inList, elements)
}
