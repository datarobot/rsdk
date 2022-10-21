#' Retrieve information about all training prediction datasets in a project.
#'
#' @inheritParams DeleteProject
#' @return data.frame containing information about each training prediction.
#' @examples
#' \dontrun{
#'  projectId <- "5984b4d7100d2b31c1166529"
#'  ListTrainingPredictions(projectId)
#' }
#' @export
ListTrainingPredictions <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "trainingPredictions")
  serverData <- DataRobotGET(routeString, simplifyDataFrame = FALSE)
  rows <- GetServerDataInRows(serverData)
  as.dataRobotTrainingPredictionList(rows)
}

as.dataRobotTrainingPredictionList <- function(trainingPredictions) {
  elements <- c("id", "modelId", "dataSubset")
  lapply(trainingPredictions, ApplySchema, schema = elements)
}


#' Retrieve training predictions on a specified data set.
#'
#' Training predictions are the internal out-of-fold predictions for data that was
#' used to train the model. These predictions are especially useful for creating
#' stacked models or blenders.
#'
#' @inheritParams DeleteProject
#' @param predictionId character. ID of the prediction to retrieve training
#'   predictions for.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   predictions <- ListTrainingPredictions(projectId)
#'   predictionId <- predictions[[1]]$id
#'   trainingPredictions <- GetTrainingPredictions(projectId, predictionId)
#' }
#' @export
GetTrainingPredictions <- function(project, predictionId) {
  projectId <- ValidateProject(project)
  message("Training predictions request issued: awaiting response")
  routeString <- UrlJoin("projects", projectId, "trainingPredictions", predictionId)
  serverData <- CleanServerData(DataRobotGET(routeString))
  rows <- GetTrainingPredictionRows(serverData)
  as.dataRobotTrainingPredictions(GetTrainingPredictionDataFrame(rows))
}


#' Retrieve the training predictions for a model using a job id.
#'
#' @inheritParams GetPredictionExplanationsMetadataFromJobId
#' @return A dataframe with out-of-fold predictions for the training data.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestTrainingPredictions(model, dataSubset = "all")
#'   trainingPredictions <- GetTrainingPredictionsFromJobId(projectId, jobId)
#' }
#' @export
GetTrainingPredictionsFromJobId <- function(project, jobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  message("Training predictions request issued: awaiting response")
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  serverData <- CleanServerData(WaitForAsyncReturn(routeString,
                                                   maxWait = maxWait,
                                                   failureStatuses = JobFailureStatuses))
  rows <- GetTrainingPredictionRows(serverData)
  as.dataRobotTrainingPredictions(GetTrainingPredictionDataFrame(rows))
}


#' Get training predictions for a particular model.
#'
#' Training predictions are the internal out-of-fold predictions for data that was
#' used to train the model. These predictions are especially useful for creating
#' stacked models or blenders.
#'
#' @inheritParams GetTrainingPredictionsFromJobId
#' @inheritParams RequestTrainingPredictions
#' @param model dataRobotModel. The model to get training predictions for.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   trainingPredictions <- GetTrainingPredictionsFromModel(model)
#' }
#' @export
GetTrainingPredictionsForModel <- function(model, dataSubset = "all", maxWait = 600) {
  jobId <- RequestTrainingPredictions(model, dataSubset = dataSubset)
  GetTrainingPredictionsFromJobId(model$projectId, jobId, maxWait = maxWait)
}


GetTrainingPredictionRows <- function(serverData) {
  count <- serverData$count
  rows <- serverData$data
  n <- 0
  while (length(serverData$nextPage) > 0) {
    serverData <- DataRobotGET(serverData$nextPage, addUrl = FALSE, simplifyDataFrame = FALSE)
    serverData$nextPage <- serverData$`next`
    count <- count + serverData$count
    rows <- append(rows, serverData$data)
  }
  message("Training predictions are available for ", count, " records")
  rows
}

#' Simplify the training prediction rows into a tidy format dataframe.
#' @param rows data.frame. The dataframe to tidy.
GetTrainingPredictionDataFrame <- function(rows) {
  if (!is.data.frame(rows)) { rows <- Reduce(rbind, rows) }
  predictionValues <- Reduce(rbind,
                             lapply(rows$predictionValues,
                                    function(x) stats::setNames(x$value,
                                                                paste0("class_", x$label))))
  # If there is more than one column, there are multiple classes and we want to display
  # the probabilities of each. If not, we don't care, since we already have a `prediction`
  # column.
  if (ncol(predictionValues) > 1) {
    cbind(rows[, setdiff(names(rows), "predictionValues")], predictionValues)
  } else {
    rows
  }
}

as.dataRobotTrainingPredictions <- function(trainingPredictions) {
  predictionValueNames <- grep("class_", names(trainingPredictions), value = TRUE)
  cols <- c("seriesId", "partitionId", "forecastDistance", "forecastPoint",
            "timestamp", "prediction", "rowId", predictionValueNames)
  trainingPredictions <- ApplySchema(trainingPredictions, cols)
  # Drop columns that are entirely NA
  Filter(function(x) !all(is.na(x)), trainingPredictions)
}


#' Request training predictions for a specific model.
#' @inheritParams DeleteModel
#' @param dataSubset character. What data subset would you like to predict on?
#'  Possible options are included in \code{DataSubset}. Possible options are:
#'  \itemize{
#'    \item \code{DataSubset$All} will use all available data.
#'    \item \code{DataSubset$ValidationAndHoldout} will use all data except the
#'      training set.
#'    \item \code{DataSubset$Holdout} will use only holdout data.
#'  }
#' @return job Id
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   RequestTrainingPredictions(model, dataSubset = DataSubset$All)
#' }
#' @export
RequestTrainingPredictions <- function(model, dataSubset) {
  validModel <- ValidateModel(model)
  ValidateParameterIn(dataSubset, DataSubset)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  modelName <- validModel$modelType
  routeString <- UrlJoin("projects", projectId, "trainingPredictions")
  body <- list(modelId = modelId, dataSubset = dataSubset)
  response <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  message("Training predictions requested for model ", modelName,
                " (modelId = ", modelId, ")")
  JobIdFromResponse(response)
}


#' Download training predictions on a specified data set.
#'
#' @inheritParams GetTrainingPredictions
#' @param filename character. Filename of file to save reason codes rows
#' @param encoding character. Optional. Character string A string representing the encoding
#'   to use in the output file, defaults to 'UTF-8'.
#' @return NULL, but will produce a CSV with a dataframe with out-of-fold predictions for the
#'   training data.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   predictions <- ListTrainingPredictions(projectId)
#'   predictionId <- predictions[[1]]$predictionId
#'   file <- file.path(tempdir(), "myTrainingPredictions.csv")
#'   DownloadTrainingPredictions(projectId, predictionId, file)
#' }
#' @export
DownloadTrainingPredictions <- function(project, predictionId, filename, encoding = "UTF-8") {
  trainingPredictions <- GetTrainingPredictions(project, predictionId)
  write.csv(trainingPredictions, file = filename, row.names = FALSE, fileEncoding = encoding)
}
