#' Create a deployment.
#'
#' @inheritParams RequestPredictionExplanationsInitialization
#' @param label character. The name of the deployment.
#' @param description character. Optional. A longer description of the deployment.
#' @param defaultPredictionServerId character. The ID of the prediction server to connect to.
#'   Can also be a prediction server object.
#' @inherit GetDeployment return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   predictionServer <- ListPredictionServers()[[1]]
#'   CreateDeployment(model,
#'                    label = "myDeployment",
#'                    description = "this is my deployment",
#'                    defaultPredictionServerId = predictionServer)
#' }
#' @export
CreateDeployment <- function(model, label = "", description = "",
                             defaultPredictionServerId = NULL) {
  if (is(model, "dataRobotModel")) {
    modelId <- ValidateModel(model)$modelId
  } else {
    modelId <- model
  }
  if (is(defaultPredictionServerId, "dataRobotPredictionServer")) {
    defaultPredictionServerId <- defaultPredictionServerId$id
  }
  body <- list(modelId = modelId,
               label = label,
               description = description)
  body$defaultPredictionServerId <- defaultPredictionServerId
  routeString <- UrlJoin("deployments", "fromLearningModel")
  deploymentId <- DataRobotPOST(routeString, body = body)$id
  GetDeployment(deploymentId)
}


#' List all current model deployments.
#' @param orderBy string. Optional. the order to sort the deployment list by, defaults to `label`
#' Allowed attributes to sort by are:
#'  * `label`
#'  * `serviceHealth`
#'  * `modelHealth`
#'  * `accuracyHealth`
#'  * `recentPredictions`
#'  * `lastPredictionTimestamp`
#'
#'  If the sort attribute is preceded by a hyphen, deployments will be sorted in descending
#'  order, otherwise in ascending order.
#'  For health related sorting, ascending means failing, warning, passing, unknown.
#' @param search string. Optional. Case insensitive search against deployment labels and
#'  descriptions.
#' @return A list of DataRobotDeployment objects containing:
#' \itemize{
#'  \item id character. The ID of the deployment.
#'  \item label character. The label of the deployment.
#'  \item description character. The description of the deployment.
#'  \item defaultPredictionServer list. Information on the default prediction
#'    server connected with the deployment. See \code{ListPredictionServers}
#'    for details.
#'  \item model dataRobotModel. The model associated with the deployment.
#'    See \code{GetModel} for details.
#'  \item capabilities list. Information on the capabilities of the deployment.
#'  \item predictionUsage list. Information on the prediction usage of the deployment.
#'  \item permissions list. User's permissions on the deployment.
#'  \item serviceHealth list. Information on the service health of the deployment.
#'  \item modelHealth list. Information on the model health of the deployment.
#'  \item accuracyHealth list. Information on the accuracy health of the deployment.
#' }
#' @examples
#' \dontrun{
#'   ListDeployments()
#' }
#' @export
#' @md
ListDeployments <- function(orderBy = NULL, search = NULL) {
  response <- DataRobotGET("deployments",
                           simplifyDataFrame = FALSE,
                           query = list("orderBy" = orderBy, "search" = search))
  # TODO: Remove the default batch size after https://datarobot.atlassian.net/browse/DSX-864
  response <- GetServerDataInRows(response, 20)
  response <- lapply(response, as.dataRobotDeployment)
  class(response) <- c("listOfDeployments", "listSubclass")
  response
}


#' Get information on a particular deployment.
#'
#' @param deploymentId character. The ID of the deployment.
#' @return A DataRobotDeployment object containing:
#' \itemize{
#'  \item id character. The ID of the deployment.
#'  \item label character. The label of the deployment.
#'  \item description character. The description of the deployment.
#'  \item defaultPredictionServer list. Information on the default prediction
#'    server connected with the deployment. See \code{ListPredictionServers}
#'    for details.
#'  \item model dataRobotModel. The model associated with the deployment.
#'    See \code{GetModel} for details.
#'  \item capabilities list. Information on the capabilities of the deployment.
#'  \item predictionUsage list. Information on the prediction usage of the deployment.
#'  \item permissions list. User's permissions on the deployment.
#'  \item serviceHealth list. Information on the service health of the deployment.
#'  \item modelHealth list. Information on the model health of the deployment.
#'  \item accuracyHealth list. Information on the accuracy health of the deployment.
#' }
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   GetDeployment(deploymentId)
#' }
#' @export
GetDeployment <- function(deploymentId) {
  routeString <- UrlJoin("deployments", deploymentId)
  deployment <- DataRobotGET(routeString)
  as.dataRobotDeployment(deployment)
}


#' Delete a deployment.
#'
#' @inheritParams GetDeployment
#' @return NULL
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   DeleteDeployment(deploymentId)
#' }
#' @export
DeleteDeployment <- function(deploymentId) {
  if (is.dataRobotDeployment(deploymentId)) {
    deploymentId <- deploymentId$id
  }
  routeString <- UrlJoin("deployments", deploymentId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}


as.dataRobotDeployment <- function(inList) {
  elements <- c("defaultPredictionServer", "description",
                "modelHealth", "predictionUsage",
                "capabilities", "label", "id", "model",
                "accuracyHealth", "serviceHealth", "permissions")
  out <- ApplySchema(inList, elements)
  class(out) <- "dataRobotDeployment"
  if (!is.null(out$defaultPredictionServer)) {
    out$defaultPredictionServer <- as.dataRobotPredictionServer(out$defaultPredictionServer)
  }
  out$model <- as.dataRobotModel(out$model)
  out$modelHealth <- ApplySchema(out$modelHealth,
                                 c("status", "startDate", "message", "endDate"))
  out$accuracyHealth <- ApplySchema(out$accuracyHealth,
                                    c("status", "startDate", "message", "endDate"))
  out$serviceHealth <- ApplySchema(out$serviceHealth,
                                   c("status", "startDate", "message", "endDate"))
  out$predictionUsage <- ApplySchema(out$predictionUsage,
                                     c("dailyRates", "lastTimestamp"))
  out$capabilities <- ApplySchema(out$capabilities,
                                  c("supportsFeatureDriftTracking",
                                    "supportsTargetDriftTracking",
                                    "supportsModelReplacement"))
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

#' Replace a model in a deployment with another model.
#'
#' @inheritParams GetDeployment
#' @param newModelId character. The ID of the model to use in the deployment. This model
#'   will replace the old model. You can also pass a dataRobotModel object.
#' @param replacementReason character. Optional. The reason for replacing the deployment.
#'   See \code{ModelReplacementReason} for a list of reasons.
#' @param maxWait integer. How long to wait (in seconds) for the computation to complete
#'   before returning a timeout error? (Default 600 seconds)
#' @inherit GetDeployment return
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   newModelId <- "5996f820af07fc605e81ead4"
#'   ReplaceDeployedModel(deploymentId, newModelId, ModelReplacementReason$Other)
#' }
#' @export
ReplaceDeployedModel <- function(deploymentId, newModelId, replacementReason, maxWait = 600) {
  if (is.dataRobotDeployment(deploymentId)) {
    deploymentId <- deploymentId$id
  }
  if (is(newModelId, "dataRobotModel")) {
    newModelId <- ValidateModel(newModelId)$modelId
  }
  routeString <- UrlJoin("deployments", deploymentId, "model")
  body <- list(modelId = newModelId, reason = replacementReason)
  response <- DataRobotPATCH(routeString, body = body,
                             returnRawResponse = TRUE, stopOnError = FALSE, encode = "json")
  HandleDeploymentErrors(response)
  response <- WaitForAsyncReturn(GetRedirectFromResponse(response),
                                 addUrl = FALSE,
                                 maxWait = maxWait,
                                 failureStatuses = "ERROR")
  as.dataRobotDeployment(response)
}


#' Validate a potential deployment model replacement.
#'
#' @inheritParams ReplaceDeployedModel
#' @return A validation report with:
#' \itemize{
#'   \item status character. Either PASSED or FAILED depending on whether all checks passed
#'     or not.
#'   \item message character. A message explaining the status failure, if any.
#'   \item checks list. A list of each check and the individual status.
#' }
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   newModelId <- "5996f820af07fc605e81ead4"
#'   ValidateReplaceDeployedModel(deploymentId, newModelId)
#' }
#' @export
ValidateReplaceDeployedModel <- function(deploymentId, newModelId) {
  if (is.dataRobotDeployment(deploymentId)) {
    deploymentId <- deploymentId$id
  }
  if (is(newModelId, "dataRobotModel")) {
    newModelId <- ValidateModel(newModelId)$modelId
  }
  routeString <- UrlJoin("deployments", deploymentId, "model", "validation")
  body <- list(modelId = newModelId)
  response <- DataRobotPOST(routeString, body = body, encode = "json")
  as.dataRobotDeploymentValidation(response)
}

as.dataRobotDeploymentValidation <- function(inList) {
  elements <- c("status", "message", "checks")
  out <- ApplySchema(inList, elements)
  checks <- c("targetType", "target", "notCurrentModel", "permission", "supported",
              "modelCanBeDeployed", "seriesType", "modelStatus", "featureDataTypes",
              "features")
  checkSchema <- c("status", "message")
  out$checks <- ApplySchema(out$checks, checks)
  for (check in checks) {
    out$checks[[check]] <- ApplySchema(out$checks[[check]], checkSchema)
  }
  out
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
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   myActuals <- data.frame(associationId = c("439917"),
#'                           actualValue = c("True"),
#'                           wasActedOn = c(TRUE))
#'   SubmitActuals(actuals = myActuals,
#'                 deploymentId)
#' }
#' @family deployment accuracy functions
#' @export
SubmitActuals <- function(actuals, deploymentId, batchSize=10000) {
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
    postResponse <- DataRobotPOST(routeString, body = payload,
                                  addUrl = TRUE, encode = "json", returnRawResponse = TRUE)
    WaitForAsyncReturn(GetRedirectFromResponse(postResponse),
                       addUrl = FALSE,
                       failureStatuses = "ERROR")
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
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   GetDeploymentDriftTrackingSettings(deploymentId)
#' }
#' @export
GetDeploymentDriftTrackingSettings <- function(deploymentId) {
  settings <- GetDeploymentSettings(deploymentId)
  as.dataRobotDeploymentDriftTrackingSettings(settings)
}

as.dataRobotDeploymentDriftTrackingSettings <- function(inList) {
  elements <- c("predictionIntervals", "targetDrift", "associationId", "featureDrift")
  out <- ApplySchema(inList, elements)
  out$predictionIntervals <- ApplySchema(out$predictionIntervals, c("percentiles", "enabled"))
  out$targetDrift <- ApplySchema(out$targetDrift, "enabled")
  out$featureDrift <- ApplySchema(out$featureDrift, "enabled")
  out$associationId <- ApplySchema(out$associationId, c("columnNames",
                                                        "requiredInPredictionRequests"))
  out
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
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   UpdateDeploymentDriftTrackingSettings(deploymentId, targetDriftEnabled = TRUE)
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

#' Retrieves all settings for a deployed model.
#'
#' @keywords internal
#'
#' @param deployment An S3 object representing a model deployment, or the unique ID of such a
#'   deployment.
#' @return List representing the various settings to be configured on a
#'   deployment, including:
#' \describe{
#'   \item{associationId}{object. Information on association ID for tracking deployment accuracy.
#'     See [GetDeploymentAssociationId()]}
#'   \item{challengerModels}{logical. Whether challenger models are enabled.}
#'   \item{featureDrift}{logical. Whether feature drift tracking is enabled. See
#'     [GetDeploymentDriftTrackingSettings()]}
#'   \item{humility}{logical. Whether humility rules are enabled.}
#'   \item{predictionIntervals}{object. Information on prediction intervals.}
#'   \item{predictionWarning}{object. Information on prediction warning settings.}
#'   \item{predictionsByForecastDate}{object. Information on predictions by forecast date.}
#'   \item{predictionsDataCollection}{logical. Whether predictions data is stored.}
#'   \item{targetDrift}{logical. Whether target drift tracking is enabled.}
#'   \item{segmentAnalysis}{object. Information on segment analysis settings.}
#' }
#' @family deployment configuration functions
#' @md
#' @export
GetDeploymentSettings <- function(deployment) {
  if (is.dataRobotDeployment(deployment)) {
    deployment <- deployment$id
  }
  routeString <- UrlJoin("deployments", deployment, "settings")
  response <- DataRobotGET(routeString)
  class(response) <- "dataRobotDeploymentSettings"
  response
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
  response <- PatchSettingsAndWait(deployment,
                       newSettings,
                       maxWait)
  as.dataRobotDeployment(response)
}

PatchSettingsAndWait <- function(deployment, payload, maxWait) {
  routeString <- UrlJoin("deployments", deployment, "settings")
  response <- DataRobotPATCH(routeString,
                             body = payload,
                             returnRawResponse = TRUE,
                             encode = "json")
  WaitForAsyncReturn(GetRedirectFromResponse(response),
                     addUrl = FALSE,
                     maxWait = maxWait,
                     failureStatuses = "ERROR")
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

#' List all available prediction servers.
#'
#' @return A list of DataRobotPredictionServer objects containing:
#' \itemize{
#'  \item id character. The ID of the prediction server.
#'  \item url character. The URL of the prediction server.
#'  \item dataRobotKey character. The key used to access the prediction server.
#' }
#' @examples
#' \dontrun{
#'   ListPredictionServers()
#' }
#' @export
ListPredictionServers <- function() {
  response <- DataRobotGET("predictionServers", simplifyDataFrame = FALSE)
  response <- GetServerDataInRows(response)
  response <- lapply(response, as.dataRobotPredictionServer)
  class(response) <- c("listOfPredictionServers", "listSubclass")
  response

}

as.dataRobotPredictionServer <- function(inList) {
  elements <- c("id", "url", "dataRobotKey")
  out <- inList
  out$dataRobotKey <- out[["datarobot-key"]]
  out <- ApplySchema(out, elements)
  class(out) <- "dataRobotPredictionServer"
  out
}
