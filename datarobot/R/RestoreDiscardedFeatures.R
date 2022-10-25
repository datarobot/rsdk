# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' Restore discarded during time series feature generation process features back to the project
#' After restoration features will be included into All Time Series Features and can be
#' used to create new feature lists.


#' Format restoredFeatures response
#'
#' @param inList list. See return value below for expected elements.
#' @return A named list that contains:
#' \describe{
#'   \item{featuresToRestore}{List of character strings with features which were accepted for
#'     restoration}
#'   \item{warnings}{List of character strings with warnings for the not accepted features}
#' }
as.dataRobotRestoredFeaturesInformation <- function(inList) {
  outList <- list(restoredFeatures = inList$featuresToRestore, warnings = inList$warnings)
  class(outList) <- "dataRobotRestoredFeaturesInformation"
  return(outList)
}


as.dataRobotDiscardedFeaturesInformation <- function(inList) {
  outList <- inList
  class(outList) <- "dataRobotDiscardedFeaturesInformation"
  return(outList)
}


#' Retrieve the information about discarded during time series feature generation process features.
#'
#' @param projectId Character string, the ID of the project.

#' @return An object with information:
#' \itemize{
#'   \item totalRestoreLimit Integer. The total limit indicating how many features can
#'     be restored in this project.
#'   \item remainingRestoreLimit Integer. The remaining available number of the features which can
#'     be restored in this project.
#'   \item features List of character strings giving discarded feature names. They can be used
#'     to pass into `RestoreDiscardedFeatures`
#'   \item count Integer. Discarded features count.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' discardedInfo <- RetrieveDiscardedFeaturesInformation(projectId)
#' }
#' @family Restore Discarded Features functions
#' @md
#' @export
RetrieveDiscardedFeaturesInformation <- function(projectId) {
  routeString <- UrlJoin("projects", projectId, "discardedFeatures")
  response <- DataRobotGET(routeString)
  return(as.dataRobotDiscardedFeaturesInformation(response))
}


HandleRouteResponse <- function(restoreResponse) {
  if (httr::status_code(restoreResponse) %notin% list(202L, 422L)) {
    StopIfResponseIsError(restoreResponse)
  } else {
    return(as.dataRobotRestoredFeaturesInformation(ParseReturnResponse(restoreResponse)))
  }
}

#' Restore discarded during time series feature generation process features back to the
#' project.
#'
#' @param projectId Character string, the ID of the project.
#' @param featuresToRestore List of character strings giving discarded feature names to restore.
#' @param maxWait integer. The maximum time (in seconds) to wait for the restoration to complete.

#' @return An object with restoration information:
#' \itemize{
#'   \item restoredFeatures. List of character strings giving restored feature names.
#'   \item warnings. List of character strings with the warnings about failed to restore features.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' restoredInfo <- RestoreDiscardedFeatures(projectId, featuresToRestore = list("Sales (7 day mean)"))
#' }
#' @family Restore Discarded Features functions
#' @md
#' @export
RestoreDiscardedFeatures <- function(projectId, featuresToRestore, maxWait = 60 * 10) {
  bodyData <- list(featuresToRestore = featuresToRestore)
  routeString <- UrlJoin("projects", projectId, "modelingFeatures", "fromDiscardedFeatures")
  postResponse <- DataRobotPOST(routeString,
    body = bodyData,
    addUrl = TRUE, encode = "json",
    returnRawResponse = TRUE,
    stopOnError = FALSE
  )
  formattedResponse <- HandleRouteResponse(postResponse)
  if (httr::status_code(postResponse) == 202L) {
    WaitForAsyncReturn(GetRedirectFromResponse(postResponse), addUrl = FALSE, maxWait = maxWait)
  }
  return(formattedResponse)
}
