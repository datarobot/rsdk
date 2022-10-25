# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

as.dataRobotDeployment <- function(inList) {
  out <- inList
  class(out) <- "dataRobotDeployment"
  if (!is.null(out$defaultPredictionServer)) {
    out$defaultPredictionServer <- as.dataRobotPredictionServer(out$defaultPredictionServer)
  }
  out$model <- as.dataRobotModel(out$model)
  out$permissions <- unlist(as.list(out$permissions))
  out
}

is.dataRobotDeployment <- function(deployment) {
  is(deployment, "dataRobotDeployment")
}

ParseDeploymentCheckFailures <- function(deploymentChecks) {
  failedChecks <- Filter(function(check) check$status != "passing", deploymentChecks)
  failedChecks <- lapply(failedChecks, `[[`, "message")
  failedChecks <- paste(failedChecks, collapse = ", ")
  paste("The following model deployment checks failed:", failedChecks)
}

HandleDeploymentErrors <- function(deploymentResponse) {
  if (httr::status_code(deploymentResponse) == 409L) {
    deploymentResponseContent <- httr::content(deploymentResponse, as = "text")
    deploymentResponseContent <- jsonlite::fromJSON(deploymentResponseContent)
    msg <- ParseDeploymentCheckFailures(deploymentResponseContent$checks)
    stop("Model deployment failure - ", msg)
  } else if (httr::status_code(deploymentResponse) != 202L) {
    deploymentResponseContent <- httr::content(deploymentResponse)
    stop("Model deployment failure - ", deploymentResponseContent$message)
  }
}

#' Submit actuals for processing.
#'
#' The actuals submitted will be used to calculate accuracy metrics.
#' Values are not processed immediately and may take some time to propagate through deployment
#' systems. Submission of actuals is limited to 10,000,000 actuals per hour. For time series
#' deployments, total actuals = number of actuals * number of forecast distances. For example,
#' submitting 10 actuals for a deployment with 50 forecast distances = 500 total actuals. For
#' multiclass deployments, a similar calculation is made where total actuals = number of actuals *
#' number of classes. For example, submitting 10 actuals for a deployment with 20 classes = 200
#' actuals.
#'
#' @inheritParams GetDeployment
#' @param actuals dataframe. Data that describes actual values. Any strings stored as factors will
#' be coerced to characters with \code{as.character}. Allowed columns are:
#' \itemize{
#'    \item associationId string. A unique identifier used with a prediction. Max length 128
#'      characters.
#'    \item actualValue string or numeric. The actual value of a prediction;
#'      should be numeric for deployments with regression models or string for deployments with
#'      classification model.
#'    \item wasActedOn logical. Optional. Indicates if the prediction was acted on in a way that
#'      could have affected the actual outcome.
#'    \item timestamp POSIXt. Optional. If the datetime provided does not have a timezone, we assume
#'      it is UTC.
#' }
#' @param batchSize integer. Optional. The max number of actuals in each batch request. Cannot
#'   exceed 10000.
#' @examples
#' \dontrun{
#' deploymentId <- "5e319d2e422fbd6b58a5edad"
#' myActuals <- data.frame(
#'   associationId = c("439917"),
#'   actualValue = c("True"),
#'   wasActedOn = c(TRUE)
#' )
#' SubmitActuals(
#'   actuals = myActuals,
#'   deploymentId
#' )
#' }
#' @family deployment accuracy functions
#' @export
SubmitActuals <- function(actuals, deploymentId, batchSize = 10000) {
  # It's not you, it's R: https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/
  # Force factor columns to character to allow for validation.
  factors <- sapply(actuals, is.factor)
  actuals[factors] <- lapply(actuals[factors], as.character)

  ValidateActuals(actuals)

  # Format timestamps as RFC3339
  if ("timestamp" %in% names(actuals)) {
    actuals[["timestamp"]] <- formatRFC3339Timestamp(actuals[["timestamp"]])
  }
  routeString <- UrlJoin("deployments", deploymentId, "actuals", "fromJSON")
  for (batch in split(actuals, (seq(nrow(actuals)) - 1) %/% batchSize)) {
    payload <- list(data = batch)
    postResponse <- DataRobotPOST(routeString,
      body = payload,
      addUrl = TRUE, encode = "json", returnRawResponse = TRUE
    )
    WaitForAsyncReturn(GetRedirectFromResponse(postResponse),
      addUrl = FALSE,
      failureStatuses = "ERROR"
    )
  }
}

#' Get drift tracking settings for a deployment.
#'
#' @inheritParams GetDeployment
#' @return A list with the following information on drift tracking:
#' \itemize{
#'    \item associationId
#'    \item predictionIntervals list. A list with two keys:
#'      \itemize{
#'         \item enabled. `TRUE` if prediction intervals are enabled and `FALSE` otherwise.
#'         \item percentiles list. A list of percentiles, if prediction intervals are enabled.
#'      }
#'    \item targetDrift list. A list with one key, `enabled`, which is `TRUE` if target
#'      drift is enabled, and `FALSE` otherwise.
#'    \item featureDrift list. A list with one key, `enabled`, which is `TRUE` if feature
#'      drift is enabled, and `FALSE` otherwise.
#' }
#' @examples
#' \dontrun{
#' deploymentId <- "5e319d2e422fbd6b58a5edad"
#' GetDeploymentDriftTrackingSettings(deploymentId)
#' }
#' @export
GetDeploymentDriftTrackingSettings <- function(deploymentId) {
  settings <- GetDeploymentSettings(deploymentId)
  as.dataRobotDeploymentDriftTrackingSettings(settings)
}

#' This function specifically narrows the Deployment Settings objects
#' to just those related to Drift Tracking.
#' @param inList JSON object retrieved from GetDeploymentSettings()
as.dataRobotDeploymentDriftTrackingSettings <- function(inList) {
  elements <- c("predictionIntervals", "targetDrift", "associationId", "featureDrift")
  return(ApplySchema(inList, elements))
}

#' Update drift tracking settings for a deployment.
#'
#' @inheritParams ReplaceDeployedModel
#' @param targetDriftEnabled logical. Optional. Set to TRUE to enable target drift. Set to
#'   FALSE to disable.
#' @param featureDriftEnabled logical. Optional. Set to TRUE to enable feature drift. Set to
#'   FALSE to disable.
#' @inherit GetDeploymentDriftTrackingSettings return
#' @examples
#' \dontrun{
#' deploymentId <- "5e319d2e422fbd6b58a5edad"
#' UpdateDeploymentDriftTrackingSettings(deploymentId, targetDriftEnabled = TRUE)
#' }
#' @export
UpdateDeploymentDriftTrackingSettings <- function(deploymentId, targetDriftEnabled = NULL,
                                                  featureDriftEnabled = NULL, maxWait = 600) {
  if (is.dataRobotDeployment(deploymentId)) {
    deploymentId <- deploymentId$id
  }
  body <- list()
  if (!is.null(targetDriftEnabled)) {
    body$targetDrift <- list(enabled = targetDriftEnabled)
  }
  if (!is.null(featureDriftEnabled)) {
    body$featureDrift <- list(enabled = featureDriftEnabled)
  }
  if (identical(body, list())) {
    stop("No changes to deployment drift tracking were found.")
  } else {
    UpdateDeploymentSettings(deploymentId, body, maxWait)
    GetDeploymentDriftTrackingSettings(deploymentId)
  }
}

#' Updates configuration settings for a deployed model.
#'
#' Updates the deployment settings and returns all settings, including those not
#' changed, on success.
#'
#' Marked as internal since we do not yet want to add this to the package index.
#' @keywords internal
#'
#' @inheritParams GetDeploymentSettings
#' @param newSettings List containing the settings to be modified. Any settings
#'   not explicitly defined will be unprocessed.
#' @inherit GetDeploymentSettings return
#' @family deployment configuration functions
#' @md
UpdateDeploymentSettings <- function(deployment, newSettings, maxWait) {
  if (is.dataRobotDeployment(deployment)) {
    deployment <- deployment$id
  }
  response <- PatchSettingsAndWait(
    deployment,
    newSettings,
    maxWait
  )
  as.dataRobotDeployment(response)
}

PatchSettingsAndWait <- function(deployment, payload, maxWait) {
  routeString <- UrlJoin("deployments", deployment, "settings")
  response <- DataRobotPATCH(routeString,
    body = payload,
    returnRawResponse = TRUE,
    encode = "json"
  )
  WaitForAsyncReturn(GetRedirectFromResponse(response),
    addUrl = FALSE,
    maxWait = maxWait,
    failureStatuses = "ERROR"
  )
}

#' Deployment Association ID
#'
#' The association ID of a deployment is a foreign key for your prediction
#' dataset that will be used to match up actual values with those predictions.
#' The ID should correspond to an event for which you want to track the outcome.
#'
#' These functions are convenience methods to get and set the association ID
#' settings for a deployment.
#'
#' @inheritParams GetDeploymentSettings
#' @inherit as.dataRobotDeploymentAssociationIdSettings return
#' @family deployment accuracy functions
#' @md
#' @export
GetDeploymentAssociationId <- function(deployment) {
  settings <- GetDeploymentSettings(deployment)
  as.dataRobotDeploymentAssociationIdSettings(settings)
}

#' Update the association ID of a deployment.
#'
#' @inheritParams GetDeploymentSettings
#' @param columnNames character. Optional. Name(s) of the column(s) in your
#'   dataset that will be used to map actuals to predictions and determine
#'   accuracy. Note: This cannot be changed after the model has served
#'   predictions and the API will return an error.
#' @param requiredInPredictionRequests logical. Optional. Whether the
#'   association ID is required in a prediction request.
#' @param maxWait integer. How long to wait (in seconds) for the computation to
#'   complete before returning a timeout error? (Default 600 seconds)

#' @describeIn GetDeploymentAssociationId Updates the association ID settings of
#'   a deployment. It will only update those settings that correspond to set
#'   arguments. This function will throw an error if the update fails and return
#'   the updated settings on success.
#' @export
UpdateDeploymentAssociationId <- function(deployment,
                                          columnNames = c(),
                                          requiredInPredictionRequests = NULL,
                                          maxWait = 600) {
  newSettings <- list()
  if (length(columnNames) > 0) {
    # if no changes, then pass nothing
    newSettings$associationId$columnNames <- as.list(columnNames)
  }
  if (!is.null(requiredInPredictionRequests)) {
    newSettings$associationId$requiredInPredictionRequests <- requiredInPredictionRequests
  }
  if (identical(newSettings, list())) {
    stop("No changes to association ID were found.")
  }
  UpdateDeploymentSettings(deployment, newSettings, maxWait)
  GetDeploymentAssociationId(deployment)
}

#' Association ID settings for a deployment.
#'
#' Helper method to process the response object received from the
#' `/deployments/{id}/settings` DataRobot API endpoint. See
#' [GetDeploymentSettings()].
#'
#' @param apiResponse List of deployment settings retrieved from the DataRobot
#'   API.
#' @return An object classed `dataRobotDeploymentAssociationIdSettings`
#' that contains:
#' \describe{
#'   \item{columnNames}{character. The columns that can be used as
#'   association IDs.}
#'   \item{requiredInPredictionRequests}{logical. Whether the association ID is
#'   required in a prediction request.}
#' }
#' @keywords internal
#' @md
as.dataRobotDeploymentAssociationIdSettings <- function(apiResponse) {
  # We just pull $associationId out of the settings object
  result <- apiResponse$associationId
  class(result) <- "dataRobotDeploymentAssociationIdSettings"
  result
}

as.dataRobotPredictionServer <- function(inList) {
  out <- inList
  if ("datarobot-key" %in% names(out) && "dataRobotKey" %notin% names(out)) {
    out$dataRobotKey <- out[["datarobot-key"]]
    out[["datarobot-key"]] <- NULL
  }
  class(out) <- "dataRobotPredictionServer"
  out
}
