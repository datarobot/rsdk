#' Retrieve model predictions
#'
#' This function can be used to retrieve predictions from a project and either
#' (1) a \code{predictionId} specifying the ID for the predictions desired (use
#' \code{ListPredictions} to see available predictionIds for individual prediction sets) or
#' (2) a \code{predictionJobId} that comes from a call to \code{RequestPredictions}.
#' This function will then return the predictions generated for the model and data.
#'
#' The contents of the return vector depends on the modeling
#' task - binary classification, multiclass classification, or regression;
#' whether or not the underlying data is time series, multiseries, cross-series, or not
#' time series; and the value of the `type` parameter.  For non-time-series regression tasks,
#' the type parameter is ignored and a vector of numerical predictions of the response
#' variable is returned.
#'
#' For binary classification tasks, either
#' a vector of predicted responses is returned if type has the
#' value \code{response} (the default), or a vector of probabilities
#' for the positive class is returned, if type is \code{probability}.
#' You can also fetch the raw dataframe of prediction values using \code{raw}.
#'
#' For multiclass classification tasks, \code{response} will return the predicted
#' class and \code{probability} will return the probability of each class.
#'
#' For time series tasks, `type = "raw"` will return more detailed information on the time
#' series prediction. This will also include any prediction intervals if requested.
#'
#' This function will error if the requested job has errored or
#' if it has not completed within \code{maxWait} seconds.
#'
#' @inheritParams DeleteProject
#' @param predictId character or integer. Either can be the character id of the
#'   \code{predictionId} associated with the prediction or the integer \code{predictionJobId}
#'   that is created by the call to \code{RequestPredictions}.
#' @param type character. String specifying the type of response for
#'   binary classifiers; see Details.
#' @param classPrefix character. For multiclass projects returning prediction probabilities,
#'   this prefix is prepended to each class in the header of the dataframe. Defaults to
#'   "class_".
#' @param maxWait integer. The maximum time (in seconds) to wait for the prediction job
#'   to complete.
#' @return Vector of predictions, depending on the modeling task
#' ("Binary", "Multiclass", or "Regression") and the value of the type parameter;
#' see Details.
#' @examples
#' \dontrun{
#'   # Retrieve by predictJobID
#'   dataset <- UploadPredictionDataset(project, diamonds_small)
#'   model <- ListModels(project)[[1]]
#'   modelId <- model$modelId
#'   predictJobId <- RequestPredictions(project, modelId, dataset$id)
#'   predictions <- GetPredictions(project, predictJobId)
#'   # Retrieve by predictionID
#'   predictions <- ListPredictions(project)
#'   predictions <- GetPredictions(project, predictions$predictionId[[1]])
#' }
#' @export
GetPredictions <- function(project, predictId,
                           type = "response", classPrefix = "class_", maxWait = 600) {
  ValidateParameterIn(type, c("response", "probability", "raw"), allowNULL = FALSE)
  message("request issued, waiting for predictions")
  projectId <- ValidateProject(project)

  if (IsId(predictId)) { # is a predictionId
    projectId <- ValidateProject(project)
    routeString <- UrlJoin("projects", projectId, "predictions", predictId)
    predictionResponse <- DataRobotGET(routeString)
  } else if (is.character(predictId) && length(predictId) == 1) { # is a predictionJobId
    predictJobRoute <- PredictJobRoute(projectId, predictId)
    timeoutMessage <-
      paste(sprintf("Retrieving predictions did not complete before timeout (%ss).", maxWait),
            "Try increasing the", sQuote("maxWait"), "parameter to increase the amount of time",
            "to wait for predictions.")
    predictionResponse <- tryCatch(WaitForAsyncReturn(predictJobRoute, maxWait = maxWait,
                                                      failureStatuses = JobFailureStatuses),
                                   AsyncTimeout = function(e) stop(timeoutMessage))
  } else {
    stop("Did not pass a valid predictId or predictionJobId.")
  }
  SelectDesiredPredictions(predictionResponse, type, classPrefix = classPrefix)
}

SelectDesiredPredictions <- function(parsedPredictionResponse, type, classPrefix = "class_") {
  predictDF <- parsedPredictionResponse$predictions
  predictDF <- Filter(function(x) !all(is.na(x)), predictDF) # Drop columns that are entirely NA
  if (identical(type, "raw")) { return(predictDF) }
  task <- parsedPredictionResponse$task
  if (identical(task, "Regression")) {
    preds <- predictDF$prediction
    if (is.list(preds)) { unlist(preds) } else { preds }
  } else if (identical(task, "Multiclass")) {
    message("Multiclass with labels ", paste0(unique(predictDF$prediction), collapse = ", "))
    if (identical(type, "response")) { predictDF$prediction }
    else {
      m <- do.call(rbind,
                   lapply(predictDF$predictionValues,
                          function(x) stats::setNames(x$value,
                                                      paste0(classPrefix, x$label))
                          )
                   )
      rownames(m) <- NULL
      return(as.data.frame(m))
    }
  } else if ("forecastPoint" %in% names(predictDF)) { # Binary time series
    if (identical(type, "response")) {
      preds <- predictDF$prediction
      if (is.list(preds)) { unlist(preds) } else { preds }
    } else { predictDF }
  } else { # Binary classification
    message("Binary classifier with positiveClass = ", parsedPredictionResponse$positiveClass)
    if (identical(type, "response")) { predictDF$prediction }
    else { predictDF$positiveProbability }
  }
}


#' Retrieve model predictions
#'
#' This function can be used to predict with a particular model.
#'
#' The contents of the return vector depends on the modeling
#' task - binary classification, multiclass classification, or regression;
#' whether or not the underlying data is time series, multiseries, cross-series, or not
#' time series; and the value of the \code{type} parameter. For non-time-series regression tasks,
#' the type parameter is ignored and a vector of numerical predictions of the response
#' variable is returned.
#'
#' This function will error if the requested job has errored or
#' if it has not completed within \code{maxWait} seconds.
#'
#' See \code{RequestPredictions} and \code{GetPredictions} for more details.
#'
#' @inheritParams GetPredictions
#' @inheritParams UploadPredictionDataset
#' @inheritParams DeleteModel
#' @inheritParams RequestPredictions
#' @param predictionDataset object. Either a dataframe of data to predict on or a DataRobot
#'   prediction dataset object of class \code{dataRobotPredictionDataset}.
#' @inherit GetPredictions return
#' @examples
#' \dontrun{
#'    trainIndex <- sample(nrow(iris) * 0.7)
#'    trainIris <- iris[trainIndex, ]
#'    testIris <- iris[-trainIndex, ]
#'    project <- StartProject(trainIris, "iris", target = "Petal_Width", wait = TRUE)
#'    model <- GetRecommendedModel(project)
#'    predictions <- Predict(model, testIris)
#'
#'    # Or, if prediction intervals are desired (datetime only)
#'    model <- GetRecommendedModel(datetimeProject)
#'    predictions <- Predict(model,
#'                           dataset,
#'                           includePredictionIntervals = TRUE,
#'                           predictionIntervalsSize = 100,
#'                           type = "raw")
#' }
#' @export
Predict <- function(model,
                    predictionDataset,
                    classPrefix = "class_",
                    maxWait = 600,
                    forecastPoint = NULL,
                    predictionsStartDate = NULL,
                    predictionsEndDate = NULL,
                    type = "response",
                    includePredictionIntervals = FALSE,
                    predictionIntervalsSize = NULL) {
  model <- ValidateModel(model)
  project <- model$projectId
  if (!is(predictionDataset, "dataRobotPredictionDataset")) {
    predictionDataset <- UploadPredictionDataset(project, predictionDataset,
                                                 forecastPoint = forecastPoint,
                                                 predictionsStartDate = predictionsStartDate,
                                                 predictionsEndDate = predictionsEndDate,
                                                 maxWait = maxWait)
  }
  predictJobId <- RequestPredictions(project,
                                     model$modelId,
                                     predictionDataset$id,
                                     includePredictionIntervals = includePredictionIntervals,
                                     predictionIntervalsSize = predictionIntervalsSize)
  GetPredictions(project, predictJobId,
                 type = type, classPrefix = classPrefix, maxWait = maxWait)
}


#' Retrieve model predictions using R's default S3 \code{predict} method.
#'
#' @seealso Predict
#' @param object dataRobotModel. The object of class \code{dataRobotModel} to predict with.
#' @param ... list. Additional arguments to pass to \code{Predict}
#' @examples
#' \dontrun{
#'    trainIndex <- sample(nrow(iris) * 0.7)
#'    trainIris <- iris[trainIndex, ]
#'    testIris <- iris[-trainIndex, ]
#'    project <- StartProject(trainIris, "iris", target = "Petal_Width", wait = TRUE)
#'    model <- GetRecommendedModel(project)
#'    predictions <- predict(model, testIris)
#' }
#' @export
predict.dataRobotModel <- function(object, ...) {
  Predict(object, ...)
}


#' Fetch all computed predictions for a project.
#'
#' This function itemizes the predictions available for a given project, model, and/or dataset.
#' Note that this function does not actually return the predictions. Use
#' \code{GetPredictions(projectId, predictionId)} to get the predictions for a particular
#' set of predictions.
#'
#' @inheritParams DeleteProject
#' @param modelId numeric. Optional. Filter returned predictions to only be predictions made
#'   against the model specified by this model ID.
#' @param datasetId numeric. Optional. Filter returned predictions to only be predictions made
#'   against the prediction dataset specified by this dataset ID.
#' @return A data.frame specifying:
#'   \itemize{
#'     \item projectId character. The ID of the project the predictions were made in.
#'     \item datasetId character. The dataset ID of the dataset used to make predictions
#'     \item modelId character. The model ID of the model used to make predictions.
#'     \item predictionId character. The unique ID corresponding to those predictions. Use
#'       \code{GetPredictions(projectId, predictionId)} to fetch the individual predictions.
#'     \item includesPredictionIntervals logical. Whether or not the predictions include
#'       prediction intervals. See \code{Predict} for details.
#'     \item predictionIntervalsSize integer. Optional. The size, in percent, of prediction
#'      intervals or NULL if there are no intervals. See \code{Predict} for details.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   predictions <- ListPredictions(projectId)
#' }
#' @export
ListPredictions <- function(project, modelId = NULL, datasetId = NULL) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictions")
  query <- list()
  query$modelId <- modelId
  query$datasetId <- datasetId
  response <- DataRobotGET(routeString, query = query)
  response <- GetServerDataInRows(response)
  as.dataRobotPredictionsList(response)
}

as.dataRobotPredictionsList <- function(inList) {
  elements <- c("projectId", "datasetId", "modelId", "predictionId",
                "includesPredictionIntervals", "predictionIntervalsSize")
  inList$predictionId <- inList$id
  ApplySchema(inList, elements)
}
