#' Retrieve information about jobs
#'
#' This function requests information about the jobs that go through the DataRobot queue.
#'
#' @inheritParams GetPredictJobs
#' @return A list of lists with one element for each job. The named list for
#' each job contains:
#' \itemize{
#'   \item status character. Model job status; an element of \code{JobStatus}, e.g.
#'     \code{JobStatus$Queue.}
#'   \item url character. URL to request more detail about the job.
#'   \item id character. The job id.
#'   \item jobType character. See \code{JobType} for valid values.
#'   \item projectId character. The project that contains the model.
#'   \item isBlocked logical. If TRUE, the job is blocked (cannot be executed) until its
#'     dependencies are resolved.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListJobs(projectId)
#' }
#' @export
ListJobs <- function(project, status = NULL) {
  projectId <- ValidateProject(project)
  query <- if (is.null(status)) { NULL } else { list(status = status) }
  routeString <- UrlJoin("projects", projectId, "jobs")
  jobsResponse <- DataRobotGET(routeString, query = query, simplifyDataFrame = FALSE)
  lapply(jobsResponse$jobs, as.dataRobotJob)
}


#' Request information about a job
#'
#' @inheritParams DeleteProject
#' @param jobId Character string specifying the job id
#' @return list with following elements:
#' \itemize{
#'   \item status character. Model job status; an element of \code{JobStatus}, e.g.
#'     \code{JobStatus$Queue}.
#'   \item url character. URL to request more detail about the job.
#'   \item id character. The job id.
#'   \item jobType character. See \code{JobType} for valid values.
#'   \item projectId character. The project that contains the model.
#'   \item isBlocked logical. If TRUE, the job is blocked (cannot be executed) until its
#'     dependencies are resolved.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- ListModelJobs(project)
#'   job <- initialJobs[[1]]
#'   jobId <- job$modelJobId
#'   GetJob(projectId, jobId)
#' }
#' @export
GetJob <- function(project, jobId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  response <- DataRobotGET(routeString, followLocation = FALSE)
  as.dataRobotJob(response)
}


as.dataRobotJob <- function(inList) {
  elements <- c("status",
                "url",
                "id",
                "jobType",
                "projectId",
                "isBlocked")
  ApplySchema(inList, elements)
}

#' Cancel a running job
#'
#' @param job object. The job you want to cancel (one of the items in the list returned from
#'   \code{ListJobs})
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- ListModelJobs(project)
#'   job <- initialJobs[[1]]
#'   DeleteJob(job)
#' }
#' @export
DeleteJob <- function(job) {
  if (!("url" %in% names(job))) {
    stop("The job has no `url` field. This function requires a job like from ListJobs.")
  }
  invisible(DataRobotDELETE(job$url, addUrl = FALSE))
}

JobIdFromJobLink <- function(jobLink) {
  # Same logic as used in our Python package to get the id from the link
  pathSplit <- unlist(strsplit(jobLink, "/"))
  pathSplit[length(pathSplit)]
}

JobIdFromResponse <- function(rawResponse) {
  # Gets the job id from the response to any request that puts a job in the project queue.
  JobIdFromJobLink(GetRedirectFromResponse(rawResponse))
}

#' Helper method to retrieve the redirect from the response headers to any
#' request that puts or queries for a DR job. Use this in your job-related methods
#' to simplify testing.
#' @keywords internal
GetRedirectFromResponse <- function(rawResponse) {
  return(httr::headers(rawResponse)$location)
}
