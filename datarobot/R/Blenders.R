#' Retrieve the details of a specified blender model
#'
#' This function returns a DataRobot S3 object of class
#' dataRobotModel for the model defined by project and modelId.
#'
#' @inheritParams DeleteProject
#' @param modelId character. Unique alphanumeric identifier for the blender model of interest.
#' @return An S3 object of class `dataRobotBlenderModel' summarizing all
#' available information about the model. It is a list with the following
#' components:
#' \itemize{
#'  \item modelId. character. The unique alphanumeric blender model identifier.
#'  \item modelNumber. integer. The assigned model number.
#'  \item modelType. character. The type of model, e.g. 'AVG Blender'.
#'  \item modelIds. character. List of unique identifiers for the blended
#'    models.
#'  \item blenderMethod. character. The blender method used to create this
#'    model.
#'  \item featurelistId. character. Unique alphanumeric identifier for the
#'    featurelist on which the model is based.
#'  \item processes. character. Components describing preprocessing; may
#'    include modelType.
#'  \item featurelistName. character. Name of the featurelist on which
#'    the model is based.
#'  \item blueprintId. character. The unique blueprint identifier on which the
#'    model is based.
#'  \item samplePct. numeric. The percentage of the dataset used in training the
#'    model. For projects that use datetime partitioning, this will be NA. See
#'    \code{trainingRowCount} instead.
#'  \item trainingRowCount. integer. Number of rows of the dataset used in
#'    training the model. For projects that use datetime partitioning, if
#'    specified, this defines the number of rows used to train the model and
#'    evaluate backtest scores; if unspecified, either \code{trainingDuration}
#'    or \code{trainingStartDate} and \code{trainingEndDate} was used instead.
#'  \item isFrozen. logical. Was the model created with frozen tuning parameters?
#'  \item metrics. list. The metrics associated with this model. Each element is
#'    a list with elements for each possible evaluation type (holdout,
#'    validation, and crossValidation).
#'  \item modelCategory. character. The category of model (e.g., blend, model,
#'    prime).
#'  \item projectId. character. Unique alphanumeric identifier for the project.
#'  \item projectName. character. Name of the project.
#'  \item projectTarget. character. The target variable predicted by all models
#'    in the project.
#'  \item projectMetric. character. The fitting metric optimized by all project
#'    models.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetBlenderModel(projectId, modelId)
#' }
#' @export
GetBlenderModel <- function(project, modelId) {
  # Fail if modelId is an empty string
  if (modelId == "") {
   stop("Invalid modelId specified")
  } else {
    projectId <- ValidateProject(project)
    fullProject <- GetProject(projectId)
    projectName <- fullProject$projectName
    projectTarget <- fullProject$target
    projectMetric <- fullProject$metric
    routeString <- UrlJoin("projects", projectId, "blenderModels", modelId)
    modelDetails <- DataRobotGET(routeString)
    listNames <- names(modelDetails)
    idIndex <- which(listNames == "id")
    names(modelDetails)[idIndex] <- "modelId"
    modelDetails$metrics <- ReformatMetrics(modelDetails$metrics)
    modelDetails$projectName <- projectName
    modelDetails$projectTarget <- projectTarget
    modelDetails$projectMetric <- projectMetric
    if (length(modelDetails$processes) == 0) {
      modelDetails$processes <- character(0)
    }
    as.dataRobotBlenderModel(modelDetails)
  }
}

#' Submit a job for creating blender model. Upon success, the new job will
#' be added to the end of the queue.
#'
#' This function requests the creation of a blend of several models in specified
#' DataRobot project. The function also allows the user to specify method used
#' for blending. This function returns an integer modelJobId value,
#' which can be used by the GetBlenderModelFromJobId function to return the full
#' blender model object.
#'
#' @inheritParams DeleteProject
#' @param modelsToBlend character. Vector listing the model Ids to be blended.
#' @param blendMethod character. Parameter specifying blending method.
#'   See acceptable values within BlendMethods.
#' @return An integer value that can be used as the modelJobId parameter
#' in subsequent calls to the GetBlenderModelFromJobId function.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelsToBlend <- c("5996f820af07fc605e81ead4", "59a5ce3301e9f0296721c64c")
#' RequestBlender(projectId, modelsToBlend, "GLM")
#' }
#' @export
RequestBlender <- function(project, modelsToBlend, blendMethod) {
  projectId <- ValidateProject(project)
  if (blendMethod == BlendMethods$FORECAST_DISTANCE) {
    Deprecated("BlendMethods$FORECAST_DISTANCE (use BlendMethods$FORECAST_DISTANCE_ENET instead)",
               "2.18", "2.19")
  }
  routeString <- UrlJoin("projects", projectId, "blenderModels")
  body <- list(modelIds = I(modelsToBlend), blenderMethod = blendMethod)
  postResponse <- DataRobotPOST(routeString,
    body = body,
    returnRawResponse = TRUE, encode = "json"
  )
  message("New blender request received")
  JobIdFromResponse(postResponse)
}

#' Retrieve a new or updated blender model defined by modelJobId
#'
#' The function RequestBlender initiates the creation of new blender models in a
#' DataRobot project.
#'
#' It submits requests to the DataRobot modeling
#' engine and returns an integer-valued modelJobId. The
#' GetBlenderModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotBlenderModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete. Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' @inheritParams DeleteProject
#' @param modelJobId integer. The integer returned by RequestBlender.
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to
#'   complete.
#' @inherit GetBlenderModel return
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelsToBlend <- c("5996f820af07fc605e81ead4", "59a5ce3301e9f0296721c64c")
#' blendJobId <- RequestBlender(projectId, modelsToBlend, "GLM")
#' GetBlenderModelFromJobId(projectId, blendJobId)
#' }
#' @export
GetBlenderModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Blender Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString,
    maxWait = maxWait,
    failureStatuses = JobFailureStatuses
  )
  modelId <- modelDetails$id
  returnModel <- GetBlenderModel(projectId, modelId)
  message("Blender Model ", modelId, " retrieved")
  class(returnModel) <- "dataRobotBlenderModel"
  returnModel
}

as.dataRobotBlenderModel <- function(inList) {
  elements <- c("featurelistId",
                "processes",
                "featurelistName",
                "projectId",
                "projectName",
                "projectTarget",
                "samplePct",
                "trainingRowCount",
                "isFrozen",
                "modelType",
                "projectMetric",
                "metrics",
                "modelCategory",
                "blueprintId",
                "modelIds",
                "blenderMethod",
                "modelId",
                "modelNumber")
  outList <- ApplySchema(inList, elements)
  class(outList) <- "dataRobotBlenderModel"
  outList
}


#' Check whether individual models can be blended together
#'
#' @inheritParams DeleteProject
#' @param modelIds list. A list of model ids corresponding to the models to check.
#' @param blendMethod character. The blender method to check. See \code{BlendMethods}.
#' @return List with:
#'   \itemize{
#'     \item blendable logical. Whether or not the models can be blended.
#'     \item reason character. An explanation for why the models cannot be blended, if not
#'       blendable. Otherwise \code{""}.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelsToBlend <- c("5996f820af07fc605e81ead4", "59a5ce3301e9f0296721c64c")
#'   IsBlenderEligible(projectId, modelId, "GLM")
#' }
#' @export
IsBlenderEligible <- function(project, modelIds, blendMethod) {
  projectId <- ValidateProject(project)
  if (blendMethod == BlendMethods$FORECAST_DISTANCE) {
     Deprecated("BlendMethods$FORECAST_DISTANCE (use BlendMethods$FORECAST_DISTANCE_ENET instead)",
                "2.18", "2.19")
  }
  routeString <- UrlJoin("projects", projectId, "blenderModels", "blendCheck")
  body <- list(modelIds = modelIds, blenderMethod = blendMethod)
  response <- DataRobotPOST(routeString, body = body, encode = "json")
  ApplySchema(response, c("reason", "blendable"))
}
