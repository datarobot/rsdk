#' DataRobot S3 object methods for R's generic as.data.frame function
#'
#' These functions extend R's generic as.data.frame function to the
#' DataRobot S3 object classes listOfBlueprints, listOfFeaturelists,
#' listOfModels, and projectSummaryList.
#'
#' All of the DataRobot S3 `listOf' class objects have relatively
#' complex structures and are often easier to work with as dataframes.
#' The methods described here extend R's generic as.data.frame function
#' to convert objects of these classes to convenient dataframes.  For
#' objects of class listOfBlueprints and listOfFeaturelists or objects
#' of class listOfModels and projectSummaryList with simple = FALSE,
#' the dataframes contain all information from the original S3 object.
#' The default value simple = TRUE provides simpler dataframes for
#' objects of class listOfModels and projectSummaryList.
#'
#'  If simple = TRUE (the default), this method returns a dataframe with
#'  one row for each model and the following columns: modelType, expandedModel
#'  (constructed from modelType and processes from the listOfModels elements),
#'  modelId, blueprintId, featurelistName, featurelistId, samplePct, and the
#'  metrics validation value for projectMetric.  If simple = FALSE, the method
#'  returns a complete dataframe with one row for each model and columns
#'  constructed from all fields in the original listOfModels object
#'
#' @param x S3 object to be converted into a dataframe.
#' @param row.names character. Optional. Row names for the dataframe returned by
#'   the method.
#' @param optional logical. Optional. If TRUE, setting row
#'   names and converting column names to syntactic names: see help for
#'   \code{make.names} function.
#' @param simple logical. Optional. if TRUE (the default), a
#'   simplified dataframe is returned for objects of class listOfModels
#'   or projectSummaryList.
#' @param \dots list. Additional optional parameters to be passed to the
#'   generic as.data.frame function (not used at present).
#' @return A dataframe containing some or all of the data from the
#'   original S3 object; see Details.
#' @name as.data.frame
NULL


#' @rdname as.data.frame
#' @export
as.data.frame.listOfBlueprints <- function(x, row.names = NULL,
                                           optional = FALSE, ...) {
  nList <- length(x)
  if (nList == 0) {
    return(data.frame(projectId = character(), modelType = character(),
                      expandedModel = character(),
                      blueprintId = character(),
                      stringsAsFactors = FALSE))
  }

  sumFrame <- NULL
  for (i in 1:nList) {
    modelType <- x[[i]]$modelType
    components <- union(modelType, x[[i]]$processes)
    expandedModel <- paste(components, collapse = "::")
    blueprintId <- x[[i]]$blueprintId
    projectId <- x[[i]]$projectId
    upFrame <- data.frame(projectId = projectId, modelType = modelType,
                          expandedModel = expandedModel,
                          blueprintId = blueprintId,
                          stringsAsFactors = FALSE)
    sumFrame <- rbind.data.frame(sumFrame, upFrame)
  }
  if (!is.null(row.names)) {
    rownames(sumFrame) <- row.names
  }
  sumFrame
}

#' @rdname as.data.frame
#' @export
as.data.frame.listOfFeaturelists <- function(x, row.names = NULL,
                                             optional = FALSE, ...) {
  nList <- length(x)
  if (nList == 0) {
    upFrame <- data.frame(featurelistId = character(),
                          projectId = character(),
                          features = I(list()),
                          name = character(),
                          stringsAsFactors = FALSE)
    class(upFrame$features) <- "list"
    return(upFrame)
  }

  sumFrame <- NULL
  for (i in 1:nList) {
    upFrame <- as.data.frame(x[[i]], stringsAsFactors = FALSE)
    sumFrame <- rbind.data.frame(sumFrame, upFrame)
  }
  if (!is.null(row.names)) {
    rownames(sumFrame) <- row.names
  }
  sumFrame
}

#' @rdname as.data.frame
#' @export
as.data.frame.listOfModels <- function(x, row.names = NULL,
                                       optional = FALSE, simple = TRUE, ...) {
  if (!is.logical(simple)) { stop("simple must be TRUE or FALSE") }
  nList <- length(x)
  if (nList == 0) {
    return(data.frame(modelType = character(),
                      expandedModel = character(),
                      modelId = character(),
                      blueprintId = character(),
                      featurelistName = character(),
                      featurelistId = character(),
                      samplePct = numeric(),
                      validationMetric = numeric(),
                      stringsAsFactors = FALSE))
  }

  outFrame <- NULL
  if (isTRUE(simple)) {
    for (i in 1:nList) {
      element <- x[[i]]
      modelType <- element$modelType
      components <- union(modelType, element$processes)
      expandedModel <- paste(components, collapse = "::")
      modelId <- element$modelId
      blueprintId <- element$blueprintId
      featurelistName <- element$featurelistName
      if (is.null(featurelistName)) { featurelistName <- "Multiple featurelists" }
      featurelistId <- element$featurelistId
      if (is.null(featurelistId)) { featurelistId <- "Multiple featurelist ids" }
      samplePct <- element$samplePct
      if (is.null(samplePct)) { samplePct <- NA }
      metricToReturn <- element$projectMetric
      allMetrics <- element$metrics
      metricIndex <- which(names(allMetrics) == metricToReturn)
      if (length(metricIndex) > 0) {
        validationMetric <- allMetrics[[metricIndex]]$validation
      } else {
        validationMetric <- NA
      }
      upFrame <- data.frame(modelType = modelType,
                            expandedModel = expandedModel,
                            modelId = modelId, blueprintId = blueprintId,
                            featurelistName = featurelistName,
                            featurelistId = featurelistId,
                            samplePct = samplePct,
                            validationMetric = validationMetric,
                            stringsAsFactors = FALSE)
      outFrame <- rbind.data.frame(outFrame, upFrame)
    }
    if (!is.null(row.names)) {
      rownames(outFrame) <- row.names
    }
  } else {
    for (i in 1:nList) {
      element <- x[[i]]
      modelType <- element$modelType
      components <- union(modelType, element$processes)
      expandedModel <- paste(components, collapse = "::")
      modelId <- element$modelId
      blueprintId <- element$blueprintId
      featurelistName <- element$featurelistName
      if (is.null(featurelistName)) { featurelistName <- "Multiple featurelists" }
      featurelistId <- element$featurelistId
      if (is.null(featurelistId)) { featurelistId <- "Multiple featurelist ids" }
      samplePct <- element$samplePct
      if (is.null(samplePct)) { samplePct <- NA }
      modelCategory <- element$modelCategory
      projectName <- element$projectName
      projectId <- element$projectId
      projectTarget <- element$projectTarget
      projectMetric <- element$projectMetric
      firstFrame <- data.frame(modelType = modelType,
                               expandedModel = expandedModel,
                               modelId = modelId, blueprintId = blueprintId,
                               featurelistName = featurelistName,
                               featurelistId = featurelistId,
                               samplePct = samplePct,
                               modelCategory = modelCategory,
                               projectName = projectName,
                               projectId = projectId,
                               projectTarget = projectTarget,
                               projectMetric = projectMetric,
                               stringsAsFactors = FALSE)
      validFrame <- BuildMetricFrame(element, "validation")
      colnames(validFrame) <- paste(colnames(validFrame), "validation",
                                    sep = ".")
      crossFrame <- BuildMetricFrame(element, "crossValidation")
      colnames(crossFrame) <- paste(colnames(crossFrame),
                                    "crossValidation", sep = ".")
      holdFrame <- BuildMetricFrame(element, "holdout")
      colnames(holdFrame) <- paste(colnames(holdFrame), "holdout", sep = ".")
      secondFrame <- cbind.data.frame(validFrame, crossFrame, holdFrame)
      upFrame <- cbind.data.frame(firstFrame, secondFrame)
      outFrame <- rbind.data.frame(outFrame, upFrame)
    }
    if (!is.null(row.names)) {
      rownames(outFrame) <- row.names
    }
  }
  outFrame
}


#  This function builds a dataframe that summarizes all of the metrics
#  included in the metrics element of the dataRobotModel object model
BuildMetricFrame <- function(model, evaluation) {
  metrics <- model$metrics
  metricNames <- names(metrics)
  n <- length(metricNames)
  oneMetric <- metrics[[1]]
  evals <- names(oneMetric)
  evalIndex <- which(evals == evaluation)
  metricFrame <- data.frame(oneMetric[evalIndex], stringsAsFactors = FALSE)
  if (n > 1) {
    for (i in 2:n) {
      oneMetric <- metrics[[i]]
      evals <- names(oneMetric)
      evalIndex <- which(evals == evaluation)
      upFrame <- data.frame(oneMetric[evalIndex], stringsAsFactors = FALSE)
      metricFrame <- cbind.data.frame(metricFrame, upFrame)
    }
  }
  colnames(metricFrame) <- metricNames
  metricFrame
}


#' Convert the project summary list to a dataframe
#'
#' If simple = TRUE (the default), this method returns a dataframe with
#' one row for each model and the following columns: projectName, projectId,
#' created, fileName, target, targetType, positiveClass, metric,
#' autopilotMode, stage, maxTrainPct, and holdoutUnlocked.
#' If simple = FALSE, a dataframe is constructed from all elements of
#' projectSummaryList.
#'
#' @rdname as.data.frame
#' @export
as.data.frame.projectSummaryList <- function(x, row.names = NULL,
                                             optional = FALSE,
                                             simple = TRUE, ...) {
  if (!is.logical(simple)) { stop("simple must be TRUE or FALSE") }
  simpleFrame <- data.frame(projectName = x$projectName,
                            projectId = x$projectId,
                            created = x$created, fileName = x$fileName,
                            target = x$target, targetType = x$targetType,
                            positiveClass = x$positiveClass, metric = x$metric,
                            autopilotMode = x$autopilotMode, stage = x$stage,
                            maxTrainPct = x$maxTrainPct,
                            holdoutUnlocked = x$holdoutUnlocked,
                            stringsAsFactors = FALSE)
  if (isTRUE(simple)) {
    outFrame <- simpleFrame
  } else {
    partFrame <- x$partition
    recFrame <- x$recommender
    advFrame <- x$advancedOptions
    outFrame <- cbind.data.frame(simpleFrame, partFrame, recFrame, advFrame,
                                 stringsAsFactors = FALSE)
  }
  if (!is.null(row.names)) {
    rownames(outFrame) <- row.names
  }
  outFrame
}


#' @rdname as.data.frame
#' @export
as.data.frame.listOfDataRobotPredictionDatasets <- function(x, row.names = NULL,
                                             optional = FALSE, ...) {
  nList <- length(x)
  if (nList == 0) {
    return(data.frame(numColumns = numeric(),
                      name = character(),
                      created = character(),
                      projectId = character(),
                      numRows = numeric(),
                      id = character(),
                      forecastPoint = numeric(),
                      predictionStartDate = numeric(),
                      predictionEndDate = numeric(),
                      stringsAsFactors = FALSE))
  }

  sumFrame <- NULL
  for (i in 1:nList) {
    # patch NULL values to NA to work with as.data.frame
    dataset <- lapply(x[[1]], function(y) if (is.null(y)) { NA } else { y })
    upFrame <- as.data.frame(dataset, stringsAsFactors = FALSE)
    sumFrame <- rbind.data.frame(sumFrame, upFrame)
  }
  if (!is.null(row.names)) {
    rownames(sumFrame) <- row.names
  }
  sumFrame
}
