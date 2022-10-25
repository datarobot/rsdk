# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Cancel a running job
#'
#' @param job object. The job you want to cancel (one of the items in the list returned from
#'   \code{ListJobs})
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' initialJobs <- ListModelJobs(project)
#' job <- initialJobs[[1]]
#' DeleteJob(job)
#' }
#' @export
DeleteJob <- function(job) {
  if ("url" %notin% names(job)) {
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
