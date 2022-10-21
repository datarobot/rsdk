#' Compute the series accuracy for a model.
#'
#' Note that you can call \code{GetSeriesAccuracy} without calling this function, and the
#' series accuracy will be requested automatically.
#'
#' @inheritParams RequestFeatureImpact
#' @return Job ID for the async job associated with the computation.
#' @examples
#' \dontrun{
#'   projectId <- "5984b4d7100d2b31c1166529"
#'   modelId <- "5984b4d7100d2b31c1166529"
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestSeriesAccuracy(projectId, modelId)
#'   WaitForJobToComplete(projectId, jobId)
#' }
#' @export
RequestSeriesAccuracy <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "datetimeModels", modelId, "multiseriesScores")
  postResponse <- DataRobotPOST(routeString, returnRawResponse = TRUE)
  JobIdFromResponse(postResponse)
}


#' Get the series accuracy associated with a particular model.
#'
#' This will not work if you have not separately computed series accuracy via
#' \code{RequestSeriesAccuracy}. See \code{GetSeriesAccuracy} for a function that
#' will get series accuracy and also compute it automatically if it has not already been compute.
#'
#' @inheritParams GetFeatureImpact
#' @return data.frame with items:
#'   \itemize{
#'     \item multiseriesId character. The ID of the series.
#'     \item rowCount integer. The number of rows in the series.
#'     \item multiseriesValues character. The name of the series.
#'     \item duration character. The duration of the series.
#'     \item validationScore numeric. The validation score for the series.
#'     \item backtestingScore numeric. The score on backtests for the series. See
#'       \code{ScoreBacktests}.
#'     \item holdoutScore numeric. The score for the series on the holdout set.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "5984b4d7100d2b31c1166529"
#'   modelId <- "5984b4d7100d2b31c1166529"
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestSeriesAccuracy(projectId, modelId)
#'   WaitForJobToComplete(projectId, jobId)
#'   seriesAccuracy <- GetSeriesAccuracyForModel(model)
#' }
#' @export
GetSeriesAccuracyForModel <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "datetimeModels", modelId, "multiseriesScores")
  serverData <- DataRobotGET(routeString, simplifyDataFrame = TRUE)
  as.dataRobotSeriesAccuracy(GetServerDataInRows(serverData))
}


#' Get the computed series accuracy for a model, computing it if not already computed.
#'
#' @inheritParams GetSeriesAccuracyForModel
#' @param maxWait integer. How long (in seconds) to wait for series accuracy computation
#'   before raising a timeout error? Default 600.
#' @inherit GetSeriesAccuracyForModel return
#' @examples
#' \dontrun{
#'   projectId <- "5984b4d7100d2b31c1166529"
#'   modelId <- "5984b4d7100d2b31c1166529"
#'   model <- GetModel(projectId, modelId)
#'   seriesAccuracy <- GetSeriesAccuracy(model)
#' }
#' @export
GetSeriesAccuracy <- function(model, maxWait = 600) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  tryCatch({
    GetSeriesAccuracyForModel(model)
  }, error = function(e) { # If need to compute...
    if (grepl("404", as.character(e))) {
      jobId <- RequestSeriesAccuracy(model)
      WaitForJobToComplete(projectId, jobId, maxWait = maxWait)
      GetSeriesAccuracyForModel(model)
    } else {
      stop(e)
    }
  })
}


#' Download the series accuracy for a model, computing it if not already computed.
#'
#' @inheritParams GetSeriesAccuracy
#' @inheritParams DownloadTrainingPredictions
#' @inherit DownloadComplianceDocumentation return
#' @examples
#' \dontrun{
#'   projectId <- "5984b4d7100d2b31c1166529"
#'   modelId <- "5984b4d7100d2b31c1166529"
#'   model <- GetModel(projectId, modelId)
#'   DownloadSeriesAccuracy(model, "seriesAccuracy.csv")
#' }
#' @export
DownloadSeriesAccuracy <- function(model, filename, encoding = "UTF-8") {
  seriesAccuracy <- GetSeriesAccuracy(model)
  write.csv(seriesAccuracy, file = filename, row.names = FALSE, fileEncoding = encoding)
  invisible(NULL)
}


as.dataRobotSeriesAccuracy <- function(inList) {
  elements <- c("multiseriesId", "validationScore", "backtestingScore", "rowCount",
                "multiseriesValues", "holdoutScore", "duration")
  output <- ApplySchema(inList, elements)
  for (col in names(output)) {
    output[[col]] <- unlist(lapply(output[[col]],
                                   function(x) if (length(x) == 0) { NA } else { x }))
  }
  output
}
