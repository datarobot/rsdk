# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Retrieve all available model information for a DataRobot project
#'
#' This function requests the model information for the DataRobot
#' project specified by the project argument, described under Arguments.
#' This parameter may be obtained in several ways, including: (1), from
#' the projectId element of the list returned by ListProjects; (2), as
#' the object returned by the GetProject function; or (3), as the list
#' returned by the SetupProject function. The function returns an S3
#' object of class 'listOfModels'.
#'
#' @inheritParams DeleteProject
#' @param orderBy character. Optional. A vector of keys to order the list by. You can
#'   order by \code{metric} or \code{samplePct}. If the sort attribute is preceded by a
#'   hyphen, models will be sorted in descending order, otherwise in ascending order.
#'   Multiple sort attributes can be included as a comma-delimited string or in a vector.
#' @param filter list. Optional. A named list of parameters to search a model
#'   by, such as \code{name}, \code{samplePct}, or \code{isStarred}.
#' @return An S3 object of class listOfModels, which may be characterized
#'   using R's generic summary function or converted to a dataframe with
#'   the as.data.frame method.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' ListModels(projectId)
#' ListModels(projectId, orderBy = c("samplePct", "-metric"))
#' ListModels(projectId, filter = list("sample_pct__gt" = 64, "name" = "Ridge"))
#' ListModels(projectId, filter = list("isStarred" = TRUE))
#' }
#' @export
ListModels <- function(project, orderBy = NULL, filter = NULL) {
  projectId <- ValidateProject(project)
  fullProject <- GetProject(projectId)
  routeString <- UrlJoin("projects", projectId, "models")
  if (length(orderBy) > 1) {
    orderBy <- paste0(orderBy, collapse = ",")
  }
  if (is.null(orderBy)) {
    orderBy <- "-metric"
  } else if (!is.character(orderBy)) {
    stop("`orderBy` must be a character vector.")
  }
  isStarred <- NULL
  if (!is.null(filter)) {
    if (!is.list(filter)) {
      stop("`filter` must be a list.")
    }
    if ("isStarred" %in% names(filter)) {
      if (isTRUE(filter$isStarred)) {
        isStarred <- "True"
      } else if (identical(filter$isStarred, FALSE)) {
        isStarred <- "False"
      } else {
        stop("`isStarred` must be logical (`TRUE` or `FALSE`).")
      }
    }
  }
  params <- list(
    "orderBy" = orderBy,
    "isStarred" = isStarred
  )
  for (i in seq_along(filter)) {
    if (names(filter)[[i]] != "isStarred") {
      params[[names(filter)[[i]]]] <- filter[[i]]
    }
  }
  modelInfo <- DataRobotGET(routeString, simplify = FALSE, query = params)
  if (length(modelInfo) == 0) {
    if (is.null(filter)) {
      message("No models have been built yet in this project.")
    }
    returnList <- list()
  } else {
    modelInfo <- lapply(modelInfo, function(model) {
      model$projectName <- fullProject$projectName
      model$projectTarget <- fullProject$target
      model$projectMetric <- fullProject$metric
      model$metrics <- ReformatMetrics(model$metrics)
      model
    })
    returnList <- lapply(modelInfo, as.dataRobotModel)
  }
  currentModelJobs <- ListModelJobs(projectId)
  if (NROW(currentModelJobs) > 0) {
    message("Some models are still in progress")
  }
  class(returnList) <- c("listOfModels", "listSubclass")
  returnList
}


#' Retrieve a new or updated model defined by modelJobId
#'
#' The functions RequestNewModel and RequestSampleSizeUpdate
#' initiate the creation of new models in a DataRobot project.
#' Both functions submit requests to the DataRobot modeling
#' engine and return an integer-valued modelJobId.  The
#' GetModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete.  Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' @inheritParams DeleteProject
#' @param modelJobId The integer returned by either RequestNewModel
#' or RequestSampleSizeUpdate.
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to complete.
#' @return An S3 object of class 'dataRobotModel' summarizing all
#' available information about the model.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' initialJobs <- ListModelJobs(project)
#' job <- initialJobs[[1]]
#' modelJobId <- job$modelJobId
#' GetModelJobFromJobId(projectId, modelJobId)
#' }
#' @export
GetModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString,
    maxWait = maxWait,
    failureStatuses = JobFailureStatuses
  )
  modelId <- modelDetails$id
  returnModel <- GetModel(projectId, modelId)
  message("Model ", modelId, " retrieved")
  class(returnModel) <- "dataRobotModel"
  returnModel
}

#' Retrieve a frozen model defined by modelJobId
#'
#' The function RequestFrozenModel
#' initiate the creation of frozen models in a DataRobot project.
#' RequestFrozenModel function submit requests to the DataRobot modeling
#' engine and return an integer-valued modelJobId.  The
#' GetFrozenModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotFrozenModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete.  Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' GetModelFromJobId also can be used to retrieve some information about
#' frozen model, however then some frozen specific information (parentModelId)
#' will not be returned.
#'
#' @inheritParams DeleteProject
#' @param modelJobId integer. The integer returned by either \code{RequestNewModel}
#'   or \code{RequestSampleSizeUpdate}.
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to
#'   complete.
#' @return An S3 object of class 'dataRobotFrozenModel' summarizing all
#' available information about the model.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' initialJobs <- ListModelJobs(project)
#' job <- initialJobs[[1]]
#' modelJobId <- job$modelJobId
#' GetModelJobFromJobId(projectId, modelJobId)
#' }
#' @export
GetFrozenModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString,
    maxWait = maxWait,
    failureStatuses = JobFailureStatuses
  )
  modelId <- modelDetails$id
  returnModel <- GetFrozenModel(projectId, modelId)
  message("Model ", modelId, " retrieved")
  returnModel
}

#' Train a new frozen model with parameters from specified model
#'
#' Frozen models use the same tuning parameters as their parent model
#' instead of independently optimizing them to allow efficiently
#' retraining models on larger amounts of the training data.
#'
#' Either `sample_pct` or `training_row_count` can be used to specify the amount of data to
#' use, but not both. If neither are specified, a default of the maximum amount of data that
#' can safely be used to train any blueprint without going into the validation data will be
#' selected.

#' In smart-sampled projects, `samplePct` and `trainingRowCount` are assumed to be in terms of rows
#' of the minority class.
#'
#' Note : For datetime partitioned projects, use `RequestFrozenDatetimeModel` instead
#'
#' @inheritParams DeleteModel
#' @param samplePct Numeric, specifying the percentage of the training
#' dataset to be used in building the new model
#' @return An integer value that can be used as the modelJobId parameter
#' in subsequent calls to the GetModelFromJobId function.
#' @param trainingRowCount integer. The number of rows to use to train
#'   the requested model.
#' @return An integer value that can be used as the modelJobId parameter
#'   in subsequent calls to the GetModelFromJobId function.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' RequestFrozenModel(model, samplePct = 10)
#' }
#' @export
RequestFrozenModel <- function(model, samplePct = NULL, trainingRowCount = NULL) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "frozenModels")
  body <- list(modelId = modelId)
  if (!is.null(samplePct)) {
    body$samplePct <- samplePct
  }
  if (!is.null(trainingRowCount)) {
    body$trainingRowCount <- trainingRowCount
  }
  postResponse <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  message("Frozen model request received")
  JobIdFromResponse(postResponse)
}


#' Delete a specified DataRobot model
#'
#' This function removes the model specified by the parameter model from its
#' associated project.
#'
#' @param model An S3 object of class dataRobotModel like that returned by
#'   the function GetModel, or each element of the list returned by
#'   the function ListModels.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' DeleteModel(model)
#' }
#' @export
DeleteModel <- function(model) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId)
  DataRobotDELETE(routeString)
  modelName <- validModel$modelType
  message(paste(
    "Model", modelName,
    "(modelId = ", modelId, ") deleted from project", projectId
  ))
  invisible(NULL)
}

as.dataRobotModel <- function(inList) {
  outList <- inList
  # rename id to modelId
  idIndex <- which(names(outList) == "id")
  names(outList)[idIndex] <- "modelId"
  class(outList) <- "dataRobotModel"
  outList
}

as.dataRobotFrozenModel <- function(inList) {
  outList <- inList
  # rename id to modelId
  idIndex <- which(names(outList) == "id")
  names(outList)[idIndex] <- "modelId"
  class(outList) <- "dataRobotFrozenModel"
  outList
}


#' Run cross validation on a model.
#'
#' Note that this runs cross validation on a model as-is. If you would like to run cross-validation
#' on a model with new parameters, use \code{RequestNewModel} instead.
#'
#' Note that this is not implemented for prime models or datetime models.
#'
#' @inheritParams DeleteModel
#' @return Job ID of the cross validation job.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' CrossValidateModel(model)
#' }
#' @export
CrossValidateModel <- function(model) {
  validModel <- ValidateAndReturnModel(model)
  if (inherits(validModel, "dataRobotPrimeModel")) {
    stop("CrossValidateModel is not implemented for prime models.")
  }
  if (inherits(validModel, "dataRobotDatetimeModel")) {
    stop("CrossValidateModel is not implemented for datetime models.")
  }
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "crossValidation")
  response <- DataRobotPOST(routeString, returnRawResponse = TRUE)
  message("Cross validation request received")
  JobIdFromResponse(response)
}

as.dataRobotModelParameters <- function(inList) {
  outList <- inList
  outList$derivedFeatures <- lapply(
    outList$derivedFeatures,
    as.dataRobotModelParametersDerivedFeatures
  )
  outList
}

as.dataRobotModelParametersDerivedFeatures <- function(inList) {
  outList <- inList
  if (is.null(outList$stageCoefficients)) {
    outList$stageCoefficients <- list()
  }
  outList
}


as.dataRobotDatetimeModel <- function(inList) {
  outList <- inList
  class(outList) <- "dataRobotDatetimeModel"
  outList
}

# helper function with logic to add monotonic feature lists to the request body
addMonotonicFeaturelist <- function(bodyFrame, featurelist, fieldName) {
  if (is.list(featurelist) &&
    "featurelistId" %in% names(featurelist)) {
    featurelist <- featurelist$featurelistId
  }
  if (!is.null(featurelist)) {
    bodyFrame[fieldName] <- featurelist
  }
  if (identical(featurelist, "")) {
    bodyFrame <- append(
      list(NULL),
      bodyFrame[setdiff(names(bodyFrame), fieldName)]
    )
    names(bodyFrame)[1] <- fieldName
  }
  bodyFrame
}

#' Retrieve a new or updated datetime model defined by modelJobId
#'
#' The functions RequestNewDatetimeModel and RequestFrozenDatetimeModel
#' initiate the creation of new models in a DataRobot project.
#' Both functions submit requests to the DataRobot modeling
#' engine and return an integer-valued modelJobId.  The
#' GetDatetimeModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotDatetimeModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete.  Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' @inheritParams DeleteProject
#' @param modelJobId The integer returned by either RequestNewDatetimeModel
#' @param maxWait Integer, The maximum time (in seconds) to wait for the model job to complete
#' @return An S3 object of class 'dataRobotDatetimeModel' summarizing all
#' available information about the model. See GetDatetimeModel
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' initialJobs <- ListModelJobs(project)
#' job <- initialJobs[[1]]
#' modelJobId <- job$modelJobId
#' GetDatetimeModelFromJobId(projectId, modelJobId)
#' }
#' @export
GetDatetimeModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString,
    maxWait = maxWait,
    failureStatuses = JobFailureStatuses
  )
  modelId <- modelDetails$id
  returnModel <- GetDatetimeModel(projectId, modelId)
  message("Model ", modelId, " retrieved")
  class(returnModel) <- "dataRobotDatetimeModel"
  returnModel
}

#' Train a new frozen datetime model with parameters from the specified model
#'
#'  Requires that this model belongs to a datetime partitioned project.
#'  If it does not, an error will occur when submitting the job
#'
#' Frozen models use the same tuning parameters as their parent model
#' instead of independently optimizing them to allow efficiently
#' retraining models on larger amounts of the training data.
#'
#' In addition to trainingRowCount and trainingDuration, frozen datetime models
#' may be trained on an exact date range.  Only one of trainingRowCount,
#' trainingDuration, or trainingStartDate and trainingEndDate should be specified.
#' Models specified using trainingStartDate and trainingEndDate are the only ones
#' that can be trained into the holdout data (once the holdout is unlocked).
#'
#' @inheritParams DeleteModel
#' @param trainingRowCount integer. (optional) the number of rows of data that
#'   should be used to train the model.
#' @param trainingDuration character. string (optional) a duration string specifying what
#'   time range the data used to train the model should span.
#' @param trainingStartDate character. string(optional) the start date of the data to train to model
#'   on ("%Y-%m-%dT%H:%M:%SZ" RFC 3339 format). Only rows occurring at or after this datetime will
#'   be used.
#' @param trainingEndDate character. string(optional) the end date of the data to train the model on
#'   ("%Y-%m-%dT%H:%M:%SZ" RFC 3339 format). Only rows occurring strictly before this datetime
#'   will be used.
#' @param timeWindowSamplePct integer. (optional) May only be specified when the requested model
#'   is a time window (e.g. duration or start and end dates). An integer between 1 and 99
#'   indicating the percentage to sample by within the window. The points kept are determined by
#'   a random uniform sample.
#' @return An integer value that can be used as the modelJobId parameter
#'   in subsequent calls to the GetDatetimeModelFromJobId function.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetDatetimeModel(modelId)
#' RequestFrozenDatetimeModel(model)
#' }
#' @export
RequestFrozenDatetimeModel <- function(model, trainingRowCount = NULL,
                                       trainingDuration = NULL, trainingStartDate = NULL,
                                       trainingEndDate = NULL, timeWindowSamplePct = NULL) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "frozenDatetimeModels")
  body <- list(
    modelId = modelId, trainingRowCount = trainingRowCount,
    trainingDuration = trainingDuration,
    trainingStartDate = trainingStartDate,
    trainingEndDate = trainingEndDate,
    timeWindowSamplePct = timeWindowSamplePct
  )
  body <- Filter(Negate(is.null), body) # Drop NULL parameters from request
  postResponse <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  message("Frozen datetime model request received")
  JobIdFromResponse(postResponse)
}



#' Compute the scores for all available backtests.
#'
#' Some backtests may be unavailable if the model is trained into their validation data.
#'
#' @inheritParams DeleteModel
#' @param wait logical. If TRUE, wait until job completion.
#' @return job ID of pending job if \code{wait} is FALSE. Use \code{WaitForJobToComplete}
#'   to await job completion. If \code{wait} is TRUE, will wait until completion and return
#'   \code{NULL}. Upon completion, all available backtests will have scores computed.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' ScoreBacktests(model)
#' }
#' @export
ScoreBacktests <- function(model, wait = FALSE) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "datetimeModels", modelId, "backtests")
  postResponse <- DataRobotPOST(routeString, returnRawResponse = TRUE)
  message("Backtest score request received")
  jobId <- JobIdFromResponse(postResponse)
  if (isTRUE(wait)) {
    WaitForJobToComplete(model$projectId, jobId)
  } else {
    jobId
  }
}

#' Download scoring code JAR
#'
#' @inheritParams DeleteProject
#' @param modelId character. Unique alphanumeric identifier for the model of interest.
#' @param fileName character. File path where scoring code will be saved.
#' @param sourceCode logical. Optional. Set to TRUE to download source code archive.
#' It will not be executable.
#' @return NULL
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' file <- file.path(tempdir(), "scoringCode.jar")
#' DownloadScoringCode(projectId, modelId, file)
#' }
#' @export
DownloadScoringCode <- function(project, modelId, fileName, sourceCode = FALSE) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "models", modelId, "scoringCode")
  query <- list("sourceCode" = tolower(as.character(sourceCode)))
  response <- DataRobotGET(routeString, as = "file", filename = fileName, query = query)
  invisible(NULL)
}


#' Get cross validation scores
#'
#' @inheritParams DeleteModel
#' @param partition numeric. Optional. The ID of the partition to filter results by.
#' @param metric character. Optional. The name of the metric to filter results by.
#' @return A list of lists with cross validation score data. Each list contains a series of lists
#'   for each model metric. Each model metric list contains the metric data for each fold.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' GetCrossValidationScores(model)
#' }
#' @export
GetCrossValidationScores <- function(model, partition = NULL, metric = NULL) {
  model <- ValidateAndReturnModel(model)
  routeString <- UrlJoin(
    "projects", model$projectId, "models",
    model$modelId, "crossValidationScores"
  )
  query <- list()
  query$partition <- partition
  query$metric <- metric
  response <- DataRobotGET(routeString, query = query)
  response$cvScores
}


#' Set a custom prediction threshold for binary classification models.
#'
#' The prediction threshold is used by a binary classification model when deciding between
#' the positive and negative class.
#'
#' Note: This feature can only can be used when \code{PredictionThresholdReadOnly} is \code{FALSE}.
#' Models typically cannot have their prediction threshold modified if they have been used to
#' set a deployment or predictions have been made with the dedicated prediction API.
#'
#' @inheritParams DeleteModel
#' @param threshold numeric. The threshold to use when deciding between the positive and
#'   negative class. Should be between 0 and 1 inclusive.
#' @return Returns NULL but updates the model in place.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' SetPredictionThreshold(model, threshold = 0.6)
#' }
#' @export
SetPredictionThreshold <- function(model, threshold) {
  model <- ValidateAndReturnModel(model)
  routeString <- UrlJoin("projects", model$projectId, "models", model$modelId)
  body <- list(predictionThreshold = threshold)
  DataRobotPATCH(routeString, body = body, encode = "json")
  invisible(NULL)
}


#' Get supported capabilities for a model, e.g., whether it has a word cloud.
#'
#' @inheritParams DeleteModel
#' @return Returns a list of logicals, representing different capabilities. Some
#'   of them are defined below:
#'   \itemize{
#'      \item supportsBlending logical. Whether the model supports blending. See
#'        \code{RequestBlender}.
#'      \item supportsMonotonicConstraints logical. Whether the model supports
#'        monotonic constraints. See \code{RequestModel}.
#'      \item supportsModelPackageExport. logical. Whether the model can be
#'        exported as a model package (a .mloc file).
#'      \item supportsCodeGeneration logical. Added in DataRobot API 2.18.
#'        Whether the model supports code generation.
#'      \item supportsShap logical. Added in DataRobot API 2.18. Whether the
#'        model supports the Shapley package, i.e. Shapley-based feature
#'        importance.
#'      \item supportsEarlyStopping. logical. Added in DataRobot API 2.22.
#'        Whether this is an early-stopping tree-based model, which denotes that
#'        the number of trained iterations can be retrieved.
#'      \item hasWordCloud logical. Whether the model has a word cloud. See
#'        \code{GetWordCloud}.
#'      \item eligibleForPrime logical. Whether the model is eligible for Prime.
#'        See \code{CreatePrimeCode}.
#'      \item hasParameters logical. Whether the model has parameters. See
#'        \code{GetModelParameters}.
#'   }
#'   The list also includes the following:
#'   \itemize{
#'      \item reasons. character. Explanations for why this model does not
#'        support certain capabilities. Not all capabilities are listed here.
#'        Names correspond to capabilities listed in \code{ModelCapability}.
#'   }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' GetModelCapabilities(model)
#' }
#' @export
GetModelCapabilities <- function(model) {
  model <- ValidateAndReturnModel(model)
  routeString <- UrlJoin(
    "projects", model$projectId, "models", model$modelId,
    "supportedCapabilities"
  )
  return(DataRobotGET(routeString))
}
