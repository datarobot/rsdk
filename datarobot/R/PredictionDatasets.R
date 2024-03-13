# Copyright 2021-2023 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' Function to upload new data to a DataRobot project for predictions
#'
#' The DataRobot prediction engine requires a CSV file containing the data to be
#' used in prediction, and this has been implemented here in two ways.
#' The first and simpler is to specify dataSource as the name of this CSV file,
#' but for the convenience of those who wish to work with dataframes, this
#' function also provides the option of specifying a dataframe, which is then
#' written to a CSV file and uploaded to the DataRobot server.
#'
#' @inheritParams DeleteProject
#' @param dataSource object. Either (a) the name of a CSV file (b) a dataframe or
#'   (c) url to publicly available file;
#'   in each case, this parameter identifies the source of the data for which
#'   predictions will be calculated.
#' @param forecastPoint character. Optional. The point relative to which predictions will be
#'     generated, based on the forecast window of the project. Only specified in time series
#'     projects.
#' @param predictionsStartDate datetime. Optional. Only specified in time series projects.
#'   The start date for bulk predictions. Note that this parameter is for generating
#'   historical predictions using the training data. This parameter should be provided in
#'   conjunction \code{predictionsEndDate}. Can't be provided with \code{forecastPoint}
#'   parameter.
#' @param predictionsEndDate datetime. Optional. Only specified in time series projects.
#'   The end date for bulk predictions. Note that this parameter is for generating
#'   historical predictions using the training data. This parameter should be provided
#'   in conjunction \code{predictionsStartDate}. Can't be provided with \code{forecastPoint}
#'   parameter.
#' @param relaxKIAFeaturesCheck logical. For time series projects only. If True, missing values
#'   in the known in advance features are allowed in the forecast window at the prediction time.
#'   If omitted or FALSE, missing values are not allowed.
#' @param maxWait integer. The maximum time (in seconds) to wait for each of two steps:
#'   (1) The initial dataset upload request, and
#'   (2) data processing that occurs after receiving the response to this initial request.
#' @return list with the following components:
#' \itemize{
#'   \item id character. The unique alphanumeric identifier for the dataset.
#'   \item numColumns numeric. Number of columns in dataset.
#'   \item name character. Name of dataset file.
#'   \item created character. time of upload.
#'   \item projectId character. String giving the unique alphanumeric identifier for the project.
#'   \item numRows numeric. Number of rows in dataset.
#'   \item forecastPoint character. The point relative to which predictions will be generated,
#'     based on the forecast window of the project. Only specified in time series projects,
#'     otherwise will be NULL.
#'   \item dataQualityWarnings list. A list of available warnings about potential problems in
#'     the uploaded prediction dataset. Will be empty if there are no warnings.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   UploadPredictionDataset(projectId, iris)
#' }
#' @export
UploadPredictionDataset <- function(project, dataSource, forecastPoint = NULL,
                                    predictionsStartDate = NULL, predictionsEndDate = NULL,
                                    relaxKIAFeaturesCheck = NULL, maxWait = 600) {
  projectId <- ValidateProject(project)
  if (isURL(dataSource)) {
    routeString <- UrlJoin("projects", projectId, "predictionDatasets", "urlUploads")
    dataList <- list(url = dataSource)
  } else {
    routeString <- UrlJoin("projects", projectId, "predictionDatasets", "fileUploads")
    dataList <- list(file = UploadData(dataSource))
  }
  if (!is.null(forecastPoint) && (!is.null(predictionsStartDate) || !is.null(predictionsEndDate))) {
    stop(sQuote("forecastPoint"), " cannot be provided along with ", sQuote("predictionsStartDate"),
         " or ", sQuote("predictionsEndDate"), ".")
  }
  if (!is.null(predictionsStartDate) && is.null(predictionsEndDate)) {
    stop("You must specify ", sQuote("predictionsEndDate"), " if you also specify ",
         sQuote("predictionsStartDate"), ".")
  }
  if (!is.null(predictionsEndDate) && is.null(predictionsStartDate)) {
    stop("You must specify ", sQuote("predictionsStartDate"), " if you also specify ",
         sQuote("predictionsEndDate"), ".")
  }
  if (!is.null(forecastPoint)) {
    dataList$forecastPoint <- forecastPoint
  }
  if (!is.null(predictionsStartDate)) {
    dataList$predictionsStartDate <- predictionsStartDate
  }
  if (!is.null(predictionsEndDate)) {
    dataList$predictionsEndDate <- predictionsEndDate
  }
  if (!is.null(relaxKIAFeaturesCheck)) {
    dataList$relaxKIAFeaturesCheck <- relaxKIAFeaturesCheck
  }
  postResponse <- DataRobotPOST(routeString, body = dataList,
                             returnRawResponse = TRUE, timeout = maxWait)
  asyncUrl <- GetRedirectFromResponse(postResponse)
  dataset <- PredictionDatasetFromAsyncUrl(asyncUrl, maxWait = maxWait)
  as.dataRobotPredictionDataset(dataset)
}


#' Upload a prediction dataset from a data source.
#'
#' @inheritParams UploadPredictionDataset
#' @inheritParams GetDataSource
#' @param username character. The username to use for authentication to the database.
#' @param password character. The password to use for authentication to the database.
#'   The password is encrypted at server side and never saved or stored.
#' @examples
#' \dontrun{
#'  dataSourceId <- "5c1303269300d900016b41a7"
#'  TestDataStore(dataSourceId, username = "myUser", password = "mySecurePass129")
#' }
#' @export
UploadPredictionDatasetFromDataSource <- function(project, dataSourceId, username, password,
                                                  forecastPoint = NULL, maxWait = 600,
                                                  relaxKIAFeaturesCheck = NULL) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionDatasets", "dataSourceUploads")
  body <- list(dataSourceId = dataSourceId,
               user = username,
               password = password)
  if (!is.null(forecastPoint)) {
    body$forecastPoint <- forecastPoint
  }
  if (!is.null(relaxKIAFeaturesCheck)) {
    body$relaxKIAFeaturesCheck <- relaxKIAFeaturesCheck
  }
  postResponse <- DataRobotPOST(routeString, body = body,
                             returnRawResponse = TRUE, timeout = maxWait)
  asyncUrl <- GetRedirectFromResponse(postResponse)
  dataset <- PredictionDatasetFromAsyncUrl(asyncUrl, maxWait = maxWait)
  as.dataRobotPredictionDataset(dataset)
}


#' Retrieve prediction dataset info from the dataset creation URL
#'
#' If dataset creation times out, the error message includes a URL corresponding to the creation
#' task. That URL can be passed to this function (which will return the completed dataset info
#' details when finished) to resume waiting for creation.
#'
#' @param asyncUrl The temporary status URL
#' @param maxWait The maximum time to wait (in seconds) for creation before aborting.
#' @export
PredictionDatasetFromAsyncUrl <- function(asyncUrl, maxWait = 600) {
  timeoutMessage <-
    paste(sprintf("Dataset creation did not complete before timeout (%ss).", maxWait),
          "To query its status and (if complete) retrieve the completed dataset info, use:\n  ",
          sprintf("%s('%s')", "PredictionDatasetFromAsyncUrl", asyncUrl))
  datasetInfo <- tryCatch(WaitForAsyncReturn(asyncUrl,
                                             addUrl = FALSE,
                                             maxWait = maxWait,
                                             failureStatuses = "ERROR"),
                          AsyncTimeout = function(e) stop(timeoutMessage))
  as.dataRobotPredictionDataset(datasetInfo)
}

#' Retrieve all prediction datasets associated with a project
#'
#' This function returns an S3 object of class listDataRobotPredictionDataset that
#' describes all prediction datasets
#' available for the project specified by the project parameter.
#' This list may be converted to a dataframe with the as.data.frame
#' method for objects of class listDataRobotPredictionDataset.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class 'listDataRobotPredictionDataset', which is a
#' list of dataframes: each element of the list corresponds to one
#' prediction dataset associated with the project, and each dataframe has
#' one row and the following columns:
#' \itemize{
#'   \item id character. The unique alphanumeric identifier for the dataset.
#'   \item numColumns numeric. Number of columns in dataset.
#'   \item name character. Name of dataset file.
#'   \item created character. time of upload.
#'   \item projectId character. String giving the unique alphanumeric identifier for the project.
#'   \item numRows numeric. Number of rows in dataset.
#'   \item forecastPoint. The point relative to which predictions will be generated, based on the
#'     forecast window of the project. Only specified in time series projects, otherwise
#'     will be NULL.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListPredictionDatasets(projectId)
#' }
#' @export
ListPredictionDatasets <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionDatasets")
  datasetInfo <- DataRobotGET(routeString, simplifyDataFrame = FALSE)
  datasetInfo <- GetServerDataInRows(datasetInfo)
  as.listOfDataRobotPredictionDatasets(datasetInfo)
}

#' Retrieve data on a prediction dataset
#'
#' @inheritParams DeleteProject
#' @param datasetId character. The ID of the prediction dataset.
#' @return Data for a particular prediction dataset:
#' \itemize{
#'   \item id character. The unique alphanumeric identifier for the dataset.
#'   \item numColumns numeric. Number of columns in dataset.
#'   \item name character. Name of dataset file.
#'   \item created character. time of upload.
#'   \item projectId character. String giving the unique alphanumeric identifier for the project.
#'   \item numRows numeric. Number of rows in dataset.
#'   \item forecastPoint. The point relative to which predictions will be generated, based on the
#'     forecast window of the project. Only specified in time series projects, otherwise
#'     will be NULL.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   datasetId <- "5cd36e6e77a90f79a28ba414"
#'   GetPredictionDataset(projectId, datasetId)
#' }
#' @export
GetPredictionDataset <- function(project, datasetId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionDatasets", datasetId)
  response <- DataRobotGET(routeString)
  as.dataRobotPredictionDataset(response)
}

as.dataRobotPredictionDataset <- function(inList) {
  elements <- c("numColumns", "name", "forecastPoint", "created", "projectId",
                "predictionsEndDate", "predictionsStartDate", "numRows", "id",
                "dataQualityWarnings")
  outList <- ApplySchema(inList, elements)
  class(outList) <- "dataRobotPredictionDataset"
  outList
}
as.listOfDataRobotPredictionDatasets <- function(inList) {
  outList <- lapply(inList, as.dataRobotPredictionDataset)
  class(outList) <- c("listOfDataRobotPredictionDatasets", "listSubclass")
  outList
}

#' Delete a specified prediction dataset
#'
#' This function removes a prediction dataset
#'
#' @inheritParams DeleteProject
#' @param datasetId The id of the dataset to delete
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   DeletePredictionDataset(projectId, datasetId)
#' }
#' @export
DeletePredictionDataset <- function(project, datasetId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionDatasets", datasetId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}


#' Request predictions from a model against a previously uploaded dataset
#'
#' Prediction intervals can now be returned for predictions with datetime models.
#' Use `includePredictionIntervals = TRUE` in calls to \code{Predict} or \code{RequestPredictions}.
#' For each model, prediction intervals estimate the range of values DataRobot expects actual values
#' of the target to fall within. They are similar to a confidence interval of a prediction, but are
#' based on the residual errors measured during the backtesting for the selected model.
#'
#' @inheritParams DeleteProject
#' @param modelId numeric. The ID of the model to use to make predictions
#' @param datasetId numeric. The ID of the dataset to make predictions against (as uploaded from
#'   \code{UploadPredictionDataset})
#' @param includePredictionIntervals logical. Optional. Should prediction intervals bounds should be
#'   part of predictions? Only available for time series projects. See "Details" for more
#'   info.
#' @param predictionIntervalsSize numeric. Optional. Size of the prediction intervals, in percent.
#'   Only available for time series projects. See "Details" for more info.
#' @return predictJobId to be used by GetPredictions function to retrieve
#'   the model predictions.
#' @examples
#' \dontrun{
#'   dataset <- UploadPredictionDataset(project, diamonds_small)
#'   model <- ListModels(project)[[1]]
#'   modelId <- model$modelId
#'   predictJobId <- RequestPredictions(project, modelId, dataset$id)
#'   predictions <- GetPredictions(project, predictJobId)
#'
#'   # Or, if prediction intervals are desired (datetime only)
#'   predictJobId <- RequestPredictions(datetimeProject,
#'                                      DatetimeModelId,
#'                                      includePredictionIntervals = TRUE,
#'                                      predictionIntervalsSize = 100)
#'   predictions <- GetPredictions(datetimeProject, predictJobId, type = "raw")
#' }
#' @export
RequestPredictions <- function(project, modelId, datasetId, includePredictionIntervals = NULL,
                               predictionIntervalsSize = NULL) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictions")
  dataList <- list(modelId = modelId, datasetId = datasetId)
  if (isTRUE(includePredictionIntervals)) {
    dataList$includePredictionIntervals <- TRUE
  }
  if (isTRUE(includePredictionIntervals) && !is.null(predictionIntervalsSize)) {
    dataList$predictionIntervalsSize <- predictionIntervalsSize
  }
  postResponse <- DataRobotPOST(routeString,
                                body = dataList,
                                returnRawResponse = TRUE,
                                encode = "json")
  JobIdFromResponse(postResponse)
}
