#' Function to list all prediction jobs in a project
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
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetPredictJobs(projectId)
#' }
#' @export
GetPredictJobs <- function(project, status = NULL) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictJobs")
  query <- if (is.null(status)) NULL else list(status = status)
  rawPredictJobStatus <- DataRobotGET(routeString, query = query)
  predictJobStatus <- rawPredictJobStatus
  predictJobId <- rawPredictJobStatus$id
  predictJobStatus$id <- NULL
  predictJobStatus$predictJobId <- predictJobId
  return(as.dataRobotPredictJobStatus(predictJobStatus))
}

PredictJobRoute <- function(projectId, predictJobId) {
  return(UrlJoin("projects", projectId, "predictJobs", predictJobId))
}


#' Request information about a predict job
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
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- GetPredictJobs(project)
#'   job <- initialJobs[[1]]
#'   predictJobId <- job$predictJobId
#'   GetPredictJob(projectId, predictJobId)
#' }
#' @export
GetPredictJob <- function(project, predictJobId) {
  projectId <- ValidateProject(project)
  routeString <- PredictJobRoute(projectId, predictJobId)
  response <- DataRobotGET(routeString, followLocation = FALSE)
  idIndex <- which(names(response) == "id")
  names(response)[idIndex] <- "predictJobId"
  as.dataRobotPredictJobStatus(response)
}


as.dataRobotPredictJobStatus <- function(inList) {
  elements <- c("status",
                "projectId",
                "modelId",
                "predictJobId")
  ApplySchema(inList, elements)
}
