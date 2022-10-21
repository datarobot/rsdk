#' Request prediction explanations initialization for specified model
#'
#' Prediction explanations initializations are a prerequisite for computing prediction
#' explanations, and include a sample of what the computed prediction explanations for a
#' prediction dataset would look like.
#'
#' @inheritParams DeleteModel
#' @return job Id
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   RequestPredictionExplanationsInitialization(model)
#' }
#' @export
RequestPredictionExplanationsInitialization <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  modelName <- validModel$modelType
  routeString <- UrlJoin("projects", projectId, "models", modelId,
                         "predictionExplanationsInitialization")
  postResponse <- DataRobotPOST(routeString, returnRawResponse = TRUE)
  message(paste("Prediction explanations initialization requested for model", modelName,
                "(modelId = ", modelId, ")"))
  JobIdFromResponse(postResponse)
}

#' Retrieve the prediction explanations initialization for a model.
#'
#' Prediction explanations initializations are a prerequisite for computing prediction
#' explanations, and include a sample what the computed prediction explanations for a
#' prediction dataset would look like.
#'
#' @inheritParams DeleteModel
#' @return A named list which contains:
#' \itemize{
#'   \item projectId character. ID of the project the feature belongs to.
#'   \item modelId character. The unique alphanumeric model identifier.
#'   \item predictionExplanationsSample list. List with sample of prediction explanations.
#'     Each element of the list is information about prediction explanations for one data row.
#'     For more information see \code{GetPredictionExplanationsRows}.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   GetPredictionExplanationsInitialization(model)
#' }
#' @export
GetPredictionExplanationsInitialization <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId,
                         "predictionExplanationsInitialization")
  as.dataRobotPredictionExplanationsInitialization(DataRobotGET(routeString,
                                                                simplifyDataFrame = FALSE))
}


as.dataRobotPredictionExplanationsInitialization <- function(inList) {
  elements <- c("projectId",
                "modelId",
                "predictionExplanationsSample")
  ApplySchema(inList, elements)
}


#' Retrieve the prediction explanations initialization for a model using jobId
#'
#' Prediction explanations initializations are a prerequisite for computing prediction
#' explanations, and include a sample what the computed prediction explanations for a
#' prediction dataset would look like.
#'
#' @inheritParams DeleteProject
#' @param jobId integer. Unique integer identifier pointing to the prediction explanations job
#' (returned for example by \code{RequestPredictionExplanationsInitialization}.)
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to complete
#' @return A named list which contains:
#' \itemize{
#'   \item projectId character. ID of the project the feature belongs to.
#'   \item modelId character. The unique alphanumeric model identifier.
#'   \item predictionExplanationsSample list. List with sample of prediction explanations.
#'     Each element of the list is information about prediction explanations for one data row.
#'     For more information see \code{GetPredictionExplanationsRows}.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestPredictionExplanationsInitialization(model)
#'   GetPredictionExplanationsInitializationFromJobId(projectId, jobId)
#' }
#' @export
GetPredictionExplanationsInitializationFromJobId <- function(project, jobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  message("Waiting for prediction explanations initialization to complete")
  predictionExplanationDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                                     failureStatuses = JobFailureStatuses)
  pseudoModel <- list(modelId = predictionExplanationDetails$modelId,
                      projectId = predictionExplanationDetails$projectId)
  class(pseudoModel) <- "dataRobotModel"
  GetPredictionExplanationsInitialization(pseudoModel)
}

#' Delete the prediction explanations initialization for a model.
#'
#' @inheritParams DeleteModel
#' @return Logical TRUE and displays a message to the user if the delete
#' request was successful; otherwise an error message is displayed.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   DeletePredictionExplanationsInitialization(model)
#' }
#' @export
DeletePredictionExplanationsInitialization <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId,
                         "predictionExplanationsInitialization")
  response <- DataRobotDELETE(routeString)
  modelName <- validModel$modelType
  message(paste("Prediction explanation initialization for model", modelName,
                "(modelId = ", modelId, ") deleted from project", projectId))
  invisible(TRUE)
}


#' Request prediction explanations computation for a specified model and dataset.
#'
#' In order to create PredictionExplanations for a particular model and dataset, you must first:
#' Compute feature impact for the model via \code{RequestFeatureImpact()}
#' Compute a PredictionExplanationsInitialization for the model via
#' \code{RequestPredictionExplanationsInitialization()}
#' Compute predictions for the model and dataset via'\code{RequestPredictions()}
#' After prediction explanations are requested information about them can be accessed using
#' the functions \code{GetPredictionExplanationsMetadataFromJobId} and
#' \code{GetPredictionExplanationsMetadata}. Prediction explanations themselves can be accessed
#' using the functions \code{GetPredictionExplanationsRows},
#' \code{GetPredictionExplanationsRowsAsDataFrame}, and \code{DownloadPredictionExplanations}.
#'
#' \code{thresholdHigh} and \code{thresholdLow} are optional filters applied to speed up
#' computation.  When at least one is specified, only the selected outlier rows will have
#' prediction explanations computed. Rows are considered to be outliers if their predicted
#' value (in case of regression projects) or probability of being the positive
#' class (in case of classification projects) is less than \code{threshold_low} or greater than
#' \code{thresholdHigh}.  If neither is specified, prediction explanations will be computed for
#' all rows.
#'
#' @inheritParams DeleteModel
#' @param datasetId character. ID of the prediction dataset for which prediction explanations
#'   are requested.
#' @param maxExplanations integer. Optional. The maximum number of prediction explanations to supply
#'   per row of the dataset, default: 3.
#' @param thresholdLow numeric. Optional. The lower threshold, below which a prediction must
#'   score in order for prediction explanations to be computed for a row in the dataset. If
#'   neither \code{threshold_high} nor \code{threshold_low} is specified, prediction
#'   explanations will be computed for all rows.
#' @param thresholdHigh numeric. Optional. The high threshold, above which a prediction must score
#'   in order for prediction explanations to be computed. If neither \code{threshold_high} nor
#'   \code{threshold_low} is specified, prediction explanations will be computed for all rows.
#' @return job Id
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(projectId, modelId)
#'   RequestPredictionExplanations(model, datasetId)
#' }
#' @export
RequestPredictionExplanations <- function(model, datasetId, maxExplanations = NULL,
                                          thresholdLow = NULL, thresholdHigh = NULL) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  modelName <- validModel$modelType
  body <- list(modelId = modelId, datasetId = datasetId)
  if (!is.null(maxExplanations)) {
    body$maxExplanations <- maxExplanations
  }
  if (!is.null(thresholdLow)) {
    body$thresholdLow <- thresholdLow
  }
  if (!is.null(thresholdHigh)) {
    body$thresholdHigh <- thresholdHigh
  }
  routeString <- UrlJoin("projects", projectId, "predictionExplanations")
  postResponse <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  message(paste("Prediction explanations requested for model", modelName,
                "(modelId = ", modelId, ")"))
  JobIdFromResponse(postResponse)
}

#' Retrieve the prediction explanations metadata for a model using jobId
#'
#' @inheritParams DeleteProject
#' @param jobId integer. Unique integer identifier (return for example by
#'   \code{RequestPredictionExplanations}).
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to complete.
#' @return A named list which contains prediction explanation metadata. For more information see
#'   \code{GetPredictionExplanationsMetadata}.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestPredictionExplanations(model, datasetId)
#'   GetPredictionExplanationsMetadataFromJobId(projectId, jobId)
#' }
#' @export
GetPredictionExplanationsMetadataFromJobId <- function(project, jobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  message("Prediction explanations request issued: awaiting response")
  predictionExplanationDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                          failureStatuses = JobFailureStatuses)
  GetPredictionExplanationsMetadata(projectId, predictionExplanationDetails$id)
}


#' Retrieve metadata for specified prediction explanations
#'
#' @inheritParams DeleteProject
#' @param predictionExplanationId character. Id of the prediction explanations.
#' @return A named list which contains prediction explanation metadata:
#' \itemize{
#'   \item id character. ID of the record and prediction explanations computation result.
#'   \item projectId character. ID of the project the model belongs to.
#'   \item modelId character. ID of the model prediction explanations initialization is for.
#'   \item datasetId character. ID of the prediction dataset prediction explanations were
#'     computed for.
#'   \item maxExplanations integer. Maximum number of prediction explanations to supply per row of
#'     the dataset.
#'   \item thresholdLow numeric. The low threshold, below which a prediction must score in order
#'     for prediction explanations to be computed for a row in the dataset.
#'   \item thresholdHigh numeric. The high threshold, above which a prediction must score in order
#'     for prediction explanations to be computed for a row in the dataset.
#'   \item numColumns integer. The number of columns prediction explanations were computed for.
#'   \item finishTime. Numeric timestamp referencing when computation for these prediction
#'     explanations finished.
#'   \item predictionExplanationsLocation character. Where to retrieve the prediction
#'     explanations.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestPredictionExplanations(model, datasetId)
#'   predictionExplanationId <- GetPredictionExplanationsMetadataFromJobId(projectId, jobId)$id
#'   GetPredictionExplanationsMetadata(projectId, predictionExplanationId)
#' }
#' @export
GetPredictionExplanationsMetadata <- function(project, predictionExplanationId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionExplanationsRecords",
                         predictionExplanationId)
  as.dataRobotPredictionExplanationsMetadata(DataRobotGET(routeString, simplifyDataFrame = FALSE))
}


as.dataRobotPredictionExplanationsMetadata <- function(inList) {
  elements <- c("id",
                "projectId",
                "modelId",
                "datasetId",
                "maxExplanations",
                "thresholdLow",
                "thresholdHigh",
                "numColumns",
                "finishTime",
                "predictionExplanationsLocation")
  ApplySchema(inList, elements)
}



#' Retrieve metadata for prediction explanations in specified project
#'
#' @inheritParams DeleteProject
#' @param modelId character. Optional. If specified, only prediction explanations computed
#'   for this model will be returned.
#' @param limit integer. Optional. At most this many results are returned, default: no limit
#' @param offset integer. This many results will be skipped, default: 0
#' @return List of metadata for all prediction explanations in the project.
#'   Each element of list is metadata for one prediction explanations
#'   (for format see \code{GetPredictionExplanationsMetadata}).
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListPredictionExplanationsMetadata(projectId)
#' }
#' @export
ListPredictionExplanationsMetadata <- function(project, modelId = NULL, limit = NULL,
                                               offset = NULL) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionExplanationsRecords")
  response <- DataRobotGET(routeString, simplifyDataFrame = FALSE,
                           body = list(modelId = modelId, limit = limit, offset = offset))
  response <- GetServerDataInRows(response)
  lapply(response, as.dataRobotPredictionExplanationsMetadata)
}


GetPredictionExplanationsPage <- function(project, predictionExplanationId, limit = NULL,
                                          offset = 0, excludeAdjustedPredictions = TRUE) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionExplanations",
                         predictionExplanationId)
  excludeAdjustedPredictions <- tolower(as.character(identical(excludeAdjustedPredictions, TRUE)))
  params <- list(offset = offset,
                 limit = limit,
                 excludeAdjustedPredictions = excludeAdjustedPredictions)
  CleanServerData(DataRobotGET(routeString,
                               simplifyDataFrame = FALSE,
                               query = params))
}

#' Retrieve all prediction explanations rows
#'
#' @inheritParams GetPredictionExplanationsRowsAsDataFrame
#' @param batchSize integer. Optional. Maximum number of prediction explanations rows to
#'   retrieve per request
#' @return list of raw prediction explanations, each element corresponds to a row of the
#'   prediction dataset and has following components.
#'    \itemize{
#'      \item rowId. Character string row Id.
#'      \item prediction. prediction for the row.
#'      \item predictionValues. list containing
#'        \itemize{
#'          \item label. describes what this model output corresponds to. For regression projects,
#'             it is the name of the target feature. For classification projects, it is a level
#'             from the  target feature.
#'          \item value. the output of the prediction.  For regression projects, it is the predicted
#'             value of the target. For classification projects, it is the predicted probability the
#'             row belongs to the class identified by the label.
#'        }
#'      \item adjustedPrediction. adjusted predictions, if they are not excluded.
#'      \item adjustedPredictionValues. Similar to predictionValues, but for adjusted predictions,
#'        if they are not excluded.
#'      \item predictionExplanations. list containing
#'        \itemize{
#'          \item label. described what output was driven by this prediction explanation.
#'            For regression projects, it is the name of the target feature. For classification
#'            projects, it is the class whose probability increasing would correspond to a
#'            positive strength of this prediction explanation.
#'          \item feature. the name of the feature contributing to the prediction.
#'          \item featureValue. the value the feature took on for this row
#'          \item strength. the amount this feature's value affected the prediction
#'          \item qualitativeStrength. a human-readable description of how strongly the feature
#'            affected the prediction (e.g. '+++', '--', '+').
#'        }
#'    }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestPredictionExplanations(model, datasetId)
#'   predictionExplanationId <- GetPredictionExplanationsMetadataFromJobId(projectId, jobId)$id
#'   GetPredictionExplanationsRows(projectId, predictionExplanationId)
#' }
#' @export
GetPredictionExplanationsRows <- function(project, predictionExplanationId, batchSize = NULL,
                                          excludeAdjustedPredictions = TRUE) {
  page <- GetPredictionExplanationsPage(project,
                                        predictionExplanationId,
                                        limit = batchSize,
                                        offset = 0,
                                        excludeAdjustedPredictions = excludeAdjustedPredictions)
  GetServerDataInRows(page, batchSize = batchSize)
}

#' Retrieve all prediction explanations rows and return them as a data frame
#'
#' There are some groups of columns whose appearance depends on the exact
#' contents of the project dataset. For classification projects,
#' columns "classNLabel", 'classNProbability", "classNLabel", "classNProbability"
#' will appear corresponding to each class within the target;
#' these columns will not appear for regression projects.
#' Columns like "explanationNLabel" will appear corresponding to each included prediction
#' explanation in the row. In both cases, the value of N will start at 1 and count up.
#'
#' @inheritParams GetPredictionExplanationsMetadata
#' @param excludeAdjustedPredictions logical. Optional. Set to FALSE to include adjusted
#'   predictions, which are predictions adjusted by an exposure column. This is only relevant for
#'   projects that use an exposure column.
#' @param batchSize integer. Optional. Maximum number of prediction explanations rows to
#'   retrieve per request
#' @return data frame with following columns:
#' \itemize{
#'   \item rowId integer. Row id from prediction dataset.
#'   \item prediction numeric. The output of the model for this row (numeric prediction for
#'     regression problem, predicted class for classification problem).
#'   \item class1Label character. Label of class 0. Available only for classification
#'     problem.
#'   \item class1Probability numeric. Predicted probability of class 0. Available only for
#'     classification problem.
#'   \item class2Label character. Label of class 1. Available only for classification
#'     problem.
#'   \item class2Probability numeric. Predicted probability of class 1. Available only for
#'     classification problem.
#'   \item explanation1FeatureName character. The name of the feature contributing to the
#'     prediction.
#'   \item explanation1FeatureValue character. the value the feature took on for this row.
#'   \item explanation1QualitativeStrength numeric. How strongly the feature affected the
#'     prediction.
#'   \item explanation1Strength character. A human-readable description of how strongly the
#'     feature affected the prediction (e.g. '+++', '--', '+').
#'   \item explanation1Label character. Describes what output was driven by this prediction
#'     explanation.
#'   For regression projects, it is the name of the target feature.  For classification projects,
#'   it is the class whose probability increasing would correspond to a positive strength of this.
#'   \item explanationNFeatureName character. The name of the feature contributing to the
#'     prediction.
#'   \item explanationNFeatureValue character. The value the feature took on for this row.
#'   \item explanationNQualitativeStrength numeric. How strongly the feature affected the
#'     prediction.
#'   \item explanationNStrength character. A human-readable description of how strongly the
#'     feature affected the prediction (e.g. '+++', '--', '+').
#'   \item explanationNLabel character. Describes what output was driven by this prediction
#'     explanation.
#'   For regression projects, it is the name of the target feature.  For classification projects,
#'   it is the class whose probability increasing would correspond to a positive strength of this.
#'   \item explanationNFeatureName. Character string the name of the feature contributing to the
#'     prediction.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestPredictionExplanations(model, datasetId)
#'   predictionExplanationId <- GetPredictionExplanationsMetadataFromJobId(projectId, jobId)$id
#'   GetPredictionExplanationsRowsAsDataFrame(projectId, predictionExplanationId)
#' }
#' @export
GetPredictionExplanationsRowsAsDataFrame <- function(project, predictionExplanationId,
                                                     excludeAdjustedPredictions = TRUE,
                                                     batchSize = NULL) {
  explains <- GetPredictionExplanationsRows(project,
                                            predictionExplanationId,
                                            batchSize = batchSize,
                                            excludeAdjustedPredictions = excludeAdjustedPredictions)
  nExplains <- length(explains)
  outDf <- data.frame(rowId = vapply(explains, `[[`, numeric(1), "rowId"),
                      predictionExplanationId = rep(predictionExplanationId, nExplains))
  predictionType <- if (is.character(explains[[1]]$prediction)) { character(1) } else { numeric(1) }
  outDf$prediction <- vapply(explains, `[[`, predictionType, "prediction")
  if (!is.null(explains[[1]]$adjustedPrediction)) {
    outDf$adjustedPrediction <- vapply(explains, `[[`, numeric(1), "adjustedPrediction")
    outDf$adjustedPredictionValues <-
      lapply(explains, function(x) x$adjustedPredictionValues[[1]])
  }

  if (length(explains) > 0) {
    if (length(explains[[1]]$predictionValues) > 1) {
      classSequence <- seq_along(explains[[1]]$predictionValues)
    } else {
      classSequence <- NULL
    }
  } else {
    classSequence <- seq(2)
  }
  if (!is.null(classSequence)) {
    for (m in classSequence) {
      # We can't make assumptions about the type of the label
      labels <- sapply(explains, function(x) x$predictionValues[[m]]$label)
      if (identical(labels, list())) { labels <- character(0) }
      outDf[paste0("class", m, "Label")] <- labels

      values <- vapply(explains, function(x) x$predictionValues[[m]]$value, numeric(1))
      outDf[paste0("class", m, "Probability")] <- values
    }
  }
  if (length(explains) > 0) {
    maxExplanations <- max(vapply(explains,
                                  function(x) length(x$predictionExplanations),
                                  numeric(1)))
  } else {
    maxExplanations <- 1
  }
  for (n in seq(maxExplanations)) {
    outDf[paste0("explanation", n, "FeatureName")] <-
      vapply(explains,
             function(x) {
               if (length(x$predictionExplanations) > 0) {
                 x$predictionExplanations[[n]]$feature
               } else { "" }}, character(1))
    featureValue <- sapply(explains,
                             function(x) {
                               if (length(x$predictionExplanations) > 0) {
                                 x$predictionExplanations[[n]]$featureValue
                               } else { "" }})
    outDf[paste0("explanation", n, "FeatureValue")] <- if (length(featureValue) > 1) {
                                                         featureValue
                                                       } else { character(0) }
    outDf[paste0("explanation", n, "QualitativeStrength")] <-
      vapply(explains,
             function(x) {
               if (length(x$predictionExplanations) > 0) {
                 x$predictionExplanations[[n]]$qualitativeStrength
               } else { "" }}, character(1))
    strength <- sapply(explains,
                         function(x) {
                           if (length(x$predictionExplanations) > 0) {
                             x$predictionExplanations[[n]]$strength
                           } else { "" }})
    outDf[paste0("explanation", n, "Strength")] <- if (length(strength) > 1) {
                                                     strength
                                                   } else { character(0) }
    outDf[paste0("explanation", n, "Label")] <-
      vapply(explains,
             function(x) {
               if (length(x$predictionExplanations) > 0) {
                 x$predictionExplanations[[n]]$label
               } else { "" }}, character(1))
  }
  outDf
}


#' Get prediction explanations
#'
#' A streamlined workflow to both generate and retrieve prediction explanations for a model.
#'
#' @inheritParams RequestPredictionExplanations
#' @inheritParams GetPredictionExplanationsMetadataFromJobId
#' @inheritParams GetPredictionExplanationsRowsAsDataFrame
#' @inherit GetPredictionExplanationsRowsAsDataFrame return
#' @param dataset object. Either (1) the prediction dataset object of class
#'   \code{dataRobotPredictionDataset}, (2) a data.frame containing the prediction data,
#'   (3) the datasetID of the prediction dataset, (4) a file path to the data, or
#'   (5) a URL to the data. References the dataset of predictions used to get prediction
#'   explanations for.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   model <- GetModel(projectId, modelId)
#'   GetPredictionExplanations(model, dataset)
#' }
#' @export
GetPredictionExplanations <- function(model, dataset, maxExplanations = NULL,
                                      thresholdLow = NULL, thresholdHigh = NULL,
                                      batchSize = NULL, maxWait = 600,
                                      excludeAdjustedPredictions = TRUE) {
  model <- ValidateModel(model)
  projectId <- model$projectId
  if (is(dataset, "dataRobotPredictionDataset")) {
    datasetId <- dataset$id
  } else if (is(dataset, "data.frame")) {
    dataset <- UploadPredictionDataset(projectId, dataset)
    datasetId <- dataset$id
  } else if (grepl(".", dataset, fixed = TRUE) ||
             grepl("/", dataset, fixed = TRUE)) { # A URL or file
    dataset <- UploadPredictionDataset(projectId, dataset)
    datasetId <- dataset$id
  } else { # A datasetId
    datasetId <- dataset
    dataset <- GetPredictionDataset(projectId, datasetId)
  }

  # Unused pre-requisites for generating prediction explanations
  Predict(model, dataset)
  GetFeatureImpact(model)

  # Run prediction explanation flow
  jobId <- RequestPredictionExplanationsInitialization(model)
  GetPredictionExplanationsInitializationFromJobId(projectId, jobId)
  jobId <- RequestPredictionExplanations(model, datasetId,
                                         maxExplanations = maxExplanations,
                                         thresholdLow = thresholdLow,
                                         thresholdHigh = thresholdHigh)
  predictionExplanationId <- GetPredictionExplanationsMetadataFromJobId(projectId, jobId,
                                                                        maxWait = maxWait)$id
  GetPredictionExplanationsRowsAsDataFrame(projectId,
                                           predictionExplanationId,
                                           batchSize = batchSize,
                                           excludeAdjustedPredictions = excludeAdjustedPredictions)
}


#' Function to download and save prediction explanations rows as csv file
#'
#' @inheritParams GetPredictionExplanationsRowsAsDataFrame
#' @param filename character. Filename of file to save prediction explanations rows
#' @param encoding character. Optional. Character string A string representing the encoding
#'   to use in the output file, defaults to 'UTF-8'.
#' @return Logical TRUE and displays a message to the user if the delete
#'   request was successful; otherwise an error message is displayed.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestPredictionExplanations(model, datasetId)
#'   predictionExplanationId <- GetPredictionExplanationsMetadataFromJobId(projectId, jobId)$id
#'   file <- file.path(tempdir(), "testPredictionExplanation.csv")
#'   DownloadPredictionExplanations(projectId, predictionExplanationId, file)
#' }
#' @export
DownloadPredictionExplanations <- function(project, predictionExplanationId, filename,
                                           encoding = "UTF-8", excludeAdjustedPredictions = TRUE) {
  predictionExplanationsFrame <- GetPredictionExplanationsRowsAsDataFrame(
                                   project,
                                   predictionExplanationId,
                                   excludeAdjustedPredictions = excludeAdjustedPredictions)
  write.csv(predictionExplanationsFrame, file = filename, row.names = FALSE,
            fileEncoding = encoding)
}

#' Function to delete prediction explanations
#'
#' This function deletes prediction explanations specified by project and
#' predictionExplanationId.
#'
#' @inheritParams GetPredictionExplanationsMetadata
#' @return Logical TRUE and displays a message to the user if the delete
#' request was successful; otherwise an error message is displayed.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestPredictionExplanations(model, datasetId)
#'   predictionExplanationId <- GetPredictionExplanationsMetadataFromJobId(projectId, jobId)$id
#'   DeletePredictionExplanations(projectId, predictionExplanationId)
#' }
#' @export
DeletePredictionExplanations <- function(project, predictionExplanationId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionExplanationsRecords",
                         predictionExplanationId)
  DataRobotDELETE(routeString)
  message(paste("Prediction explanations ", predictionExplanationId,
                "deleted from project", projectId))
  invisible(TRUE)
}
