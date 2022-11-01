# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Wait for a job or task to return
#'
#' Helper function to process async responses from the DataRobot Public API.
#'
#' The DataRobot Public API uses the following pattern for handling asynchronous requests:
#' 1. The client sends a request.
#' 2. The server constructs a task request to send to a worker, then sets up a resource the client can poll to determine when the task is finished.
#' 3. The client receives a `202 Accepted` response with a `Location` HTTP header pointing to the resource to poll.
#' 4. On the server, the worker finishes the task and updates the polling resource.
#' 5. The client, on the next poll, receives a `303 See Other` response with a new `Location` header pointing to the finished result.
#'
#' `WaitForAsyncReturn` automates most of this processing.
#'
#' @inheritParams MakeDataRobotRequest
#' @param maxWait integer. Maximum time to wait (in seconds) for asynchronous job or task to complete.
#' @param failureStatuses character. A vector of error statuses to look for. This function will terminate
#' early if any of these statuses are received in an API response.
#' @return The result of the asynchronous task, if it finishes successfully.
#' @examples{
#' \dontrun{
#' # To invoke when crafting a URL by hand
#' routeString <- UrlJoin("projects", projectId, "jobs", jobId)
#' response <- WaitForAsyncReturn(routeString, maxWait, failureStatuses = JobFailureStatuses)
#'
#'
#' # To invoke after another function call that includes the Location header in the response
#' postResponse <- DataRobotPOST(routeString, body = body, addUrl = TRUE, returnRawResponse = TRUE)
#' response <- WaitForAsyncReturn(GetRedirectFromResponse(postResponse), addUrl = FALSE)
#' }
#' }
#' @export
#' @md
WaitForAsyncReturn <- function(routeString, maxWait = 600, addUrl = TRUE, failureStatuses = c()) {
  rawStatusInfo <- httr::with_config(
    httr::config(followlocation = FALSE),
    WaitFor303(routeString, maxWait, addUrl, failureStatuses)
  )
  asyncLocation <- GetRedirectFromResponse(rawStatusInfo)
  tryCatch(DataRobotGET(asyncLocation, addUrl = FALSE, timeout = maxWait),
    error = function(e) {
      if (grepl("Expected JSON, received", e)) {
        # Allow JSON parse errors
        NULL # (happens when awaiting download jobs)
      } else {
        stop(e)
      }
    }
  )
}


WaitFor303 <- function(routeString, maxWait, addUrl = TRUE, failureStatuses) {
  GetWaitStatus <- StartRetryWaiter(timeout = maxWait, maxdelay = 1)
  while (GetWaitStatus()$stillTrying) {
    rawResponse <- DataRobotGET(routeString, addUrl = addUrl, returnRawResponse = TRUE)
    parsedResponse <- ParseReturnResponse(rawResponse)
    statusCode <- httr::status_code(rawResponse)
    StopIfResponseIsError(rawResponse)
    if (statusCode == 303) {
      return(rawResponse)
    } else if (statusCode == 200) {
      if (parsedResponse$status %in% failureStatuses) {
        Raise(Exceptions$PendingJobFailed(paste(
          "\n", "status:", parsedResponse$status,
          "\n", "message:", parsedResponse$message
        )))
      }
    } else {
      stop(sprintf("Unexpected status code %d.", statusCode))
    }
  }

  # Extra details in a message instead of in the error itself so that you still get the data
  # when calling functions catch the error and display a different message:
  message(sprintf("Async URL: %s\nLatest status: %s", routeString, parsedResponse$status))
  Raise(Exceptions$AsyncTimeout(message = "Async service timed out"))
}
