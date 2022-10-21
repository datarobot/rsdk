#' Refits an existing model to a different fraction of the training dataset
#'
#' This function requests a refit of the model defined by the model parameter
#' to the same training dataset used in building it originally, but with a
#' different fraction of the data, specified by the samplePct parameter.
#' The function returns an integer value that may be used with the function
#' GetModelFromJobId to retrieve the model after fitting is complete.
#'
#' Motivation for this function is the fact that some models - e.g., very
#' complex machine learning models fit to large datasets - may take a long
#' time to complete.  Splitting the model creation request from model retrieval
#' in these cases allows the user to perform other interactive R session tasks
#' between the time the model creation/update request is made and the time the
#' final model is available.
#'
#' Either `sample_pct` or `training_row_count` can be used to specify the amount of data to
#' use, but not both. If neither are specified, a default of the maximum amount of data that
#' can safely be used to train any blueprint without going into the validation data will be
#' selected.

#' In smart-sampled projects, `samplePct` and `trainingRowCount` are assumed to be in terms of rows
#' of the minority class.
#'
#' @inheritParams DeleteModel
#' @param samplePct Numeric, specifying the percentage of the training dataset
#' to be used in building the new model.
#' @param trainingRowCount integer. The number of rows to use to train
#'   the requested model.
#' @return Integer, value to be used as the modelJobId parameter in calling the
#' function GetModelFromJobId to retrieve the updated model.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   RequestSampleSizeUpdate(model, samplePct = 100)
#' }
#' @export
RequestSampleSizeUpdate <- function(model, samplePct = NULL, trainingRowCount = NULL) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  routeString <- UrlJoin("projects", projectId, "models")
  body <- list(blueprintId = model$blueprintId)
  if (!is.null(samplePct)) {
    body$samplePct <- samplePct
  }
  if (!is.null(trainingRowCount)) {
    body$trainingRowCount <- trainingRowCount
  }
  rawResponse <- DataRobotPOST(routeString, body = body,
                             returnRawResponse = TRUE, encode = "json")
  message("Model creation request submitted - retrieve via modelJobId
          value returned")
  modelJobPath <- GetRedirectFromResponse(rawResponse)
  pathSplit <- unlist(strsplit(modelJobPath, "modelJobs/"))
  modelJobId <- gsub("/", "", pathSplit[2])
  return(modelJobId)
}
