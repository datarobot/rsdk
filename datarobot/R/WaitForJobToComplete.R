#' Wait for specified job to complete
#'
#' @inheritParams DeleteProject
#' @param jobId integer identifier (returned for example by RequestPrimeModel)
#' @param maxWait maximum time to wait (in seconds) for the job to complete
#' @return NULL
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   blueprints <- ListBlueprints(projectId)
#'   blueprint <- blueprints[[1]]
#'   jobId <- RequestNewModel(projectId, blueprint)
#'   WaitForJobToComplete(projectId, jobId)
#' }
#' @export
WaitForJobToComplete <- function(project, jobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  WaitForAsyncReturn(routeString, maxWait, failureStatuses = JobFailureStatuses)
  invisible(NULL)
}
