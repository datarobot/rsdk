# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' Compute datetime trend plots for datetime partitioned model.
#'
#' Compute datetime trend plots for datetime partitioned model. This includes
#' Accuracy over Time, Forecast vs Actual, and Anomaly over Time plots.
#' @details
#' \itemize{
#'  \item{
#'    Forecast distance specifies the number of time steps
#'    between the predicted point and the origin point.
#'  }
#'  \item{
#'    For the multiseries models only first 1000 series in alphabetical order
#'    and an average plot for them will be computed.
#'  }
#'  \item{
#'    Maximum 100 forecast distances can be requested for
#'    calculation in time series supervised projects.
#'  }
#' }
#'
#' @inheritParams DeleteModel
#' @param backtest integer or character. Optional. Compute plots for a specific backtest.
#'   Use the backtest index starting from zero.
#'   To compute plots for holdout, use \code{DataSubset$Holdout}.
#' @param source character. Optional. The source of the data for the backtest/holdout.
#'   Must be one of \code{SourceType}.
#' @param forecastDistanceStart integer. Optional. The start of forecast distance range
#'   (forecast window) to compute. If not specified, the first forecast distance
#'   for this project will be used. Only for time series supervised models.
#' @param forecastDistanceEnd integer. Optional. The end of forecast distance range
#'   (forecast window) to compute. If not specified, the last forecast distance
#'   for this project will be used. Only for time series supervised models.
#' @return An integer value that can be used as the jobId parameter in a subsequent call
#'   to \code{WaitForJobToComplete}.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' jobId <- ComputeDatetimeTrendPlots(model)
#' WaitForJobToComplete(projectId, jobId) # optional step
#' }
#' @export
ComputeDatetimeTrendPlots <- function(model, backtest = 0,
                                      source = SourceType$Validation, forecastDistanceStart = NULL,
                                      forecastDistanceEnd = NULL) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels",
    modelId, "datetimeTrendPlots"
  )
  body <- list(
    backtest = backtest, source = source,
    forecastDistanceStart = forecastDistanceStart,
    forecastDistanceEnd = forecastDistanceEnd
  )
  body <- Filter(Negate(is.null), body) # Drop NULL parameters from request
  postResponse <- DataRobotPOST(routeString,
    body = body,
    returnRawResponse = TRUE,
  )
  JobIdFromResponse(postResponse)
}


#' Retrieve Accuracy over Time plots metadata for a model.
#'
#' @inheritParams DeleteModel
#' @param forecastDistance integer. Optional. Forecast distance to retrieve the metadata for.
#'   If not specified, the first forecast distance for this project will be used.
#'   Only available for time series projects.
#' @return list with the following components:
#' \itemize{
#'   \item forecastDistance. integer or NULL:
#'         The forecast distance for which the metadata was retrieved. NULL for OTV projects.
#'   \item resolutions. list: A list of \code{DatetimeTrendPlotsResolutions},
#'         which represents available time resolutions for which plots can be retrieved.
#'   \item backtestStatuses. data.frame: Each row represents a status for the backtest
#'         \code{SourceType}. The row index corresponds to the backtest index via the relation
#'         \code{rowIndex <- backtestIndex + 1}. Status should be one of
#'         \code{DatetimeTrendPlotsStatuses}
#'   \item backtestMetadata. data.frame: Each row represents a metadata for the backtest
#'         \code{SourceType} start and end date. The row index corresponds to the
#'         backtest index via the relation \code{rowIndex <- backtestIndex + 1}.
#'         Each cell contains a POSIXct timestamp for start date (inclusive)
#'         and end date (exclusive) if the correspoding source type
#'         for the backtest is computed, and NA otherwise.
#'   \item holdoutStatuses. list: Contains statuses for holdout.
#'   \itemize{
#'     \item training. character: Status, one of \code{DatetimeTrendPlotsStatuses}
#'     \item validation. character: Status, one of \code{DatetimeTrendPlotsStatuses}
#'   }
#'   \item holdoutMetadata. list. Contains metadata for holdout.
#'   \itemize{
#'     \item training. list. Contains start and end date for holdout training.
#'     \item validation. list. Contains start and end date for holdout validation.
#'       \itemize{
#'         \item startDate. POSIXct or NA:
#'               The datetime of the start of the holdout training/validation (inclusive).
#'               NA if the data is not computed.
#'         \item endDate. POSIXct or NA:
#'               The datetime of the end of the holdout training/validation (exclusive).
#'               NA if the data is not computed.
#'         }
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' GetAccuracyOverTimePlotsMetadata(model)
#' }
#' @export
GetAccuracyOverTimePlotsMetadata <- function(model, forecastDistance = NULL) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels",
    modelId, "accuracyOverTimePlots", "metadata"
  )
  query <- list(
    forecastDistance = forecastDistance
  )
  query <- Filter(Negate(is.null), query)
  response <- DataRobotGET(routeString, query = query, simplifyDataFrame = TRUE)
  return(as.accuracyOverTimePlotsMetadata(response))
}

parseTimestampConditionally <- function(timestamp) {
  if (!is.null(timestamp)) {
    return(ParseRFC3339Timestamp(timestamp))
  }
  return(NA)
}

as.datetimeTrendPlotsBacktestHoldoutMetadata <- function(inList) {
  outList <- inList
  outList$backtestMetadata$training[] <- lapply(
    outList$backtestMetadata$training, parseTimestampConditionally
  )
  outList$backtestMetadata$validation[] <- lapply(
    outList$backtestMetadata$validation, parseTimestampConditionally
  )
  outList$holdoutMetadata$training <- lapply(
    outList$holdoutMetadata$training, parseTimestampConditionally
  )
  outList$holdoutMetadata$validation <- lapply(
    outList$holdoutMetadata$validation, parseTimestampConditionally
  )
  return(outList)
}

as.accuracyOverTimePlotsMetadata <- function(inList) {
  outList <- inList
  outList <- as.datetimeTrendPlotsBacktestHoldoutMetadata(outList)
  outList$forecastDistance <- unlist(outList$forecastDistance)
  return(outList)
}

#' A helper function to retrieve the status for specific backtest and source
#' for Datetime trend plots metadata.
#'
#' @param metadata list. The list retrieved from one of \code{GetAccuracyOverTimePlotsMetadata},
#'   \code{GetForecastVsActualPlotsMetadata} or \code{GetAnomalyOverTimePlotsMetadata}.
#' @param backtest integer or character. Retrieve plots for a specific backtest.
#'   Use the backtest index starting from zero.
#'   To retrieve plots for holdout, use \code{DataSubset$Holdout}.
#' @param source character. The source of the data for the backtest/holdout.
#'   Must be one of \code{SourceType}.
#' @return character (Accuracy over Time, Anomaly over Time) or the list (Forecast vs Actual).
getMetadataStatus <- function(metadata, backtest, source) {
  if (backtest == DataSubset$Holdout) {
    return(metadata$holdoutStatuses[[source]])
  }
  return(metadata$backtestStatuses[backtest + 1, source])
}

computeAccuracyOverTimePlotIfNotComputed <- function(model, backtest, source,
                                                     forecastDistance, maxWait) {
  metadata <- GetAccuracyOverTimePlotsMetadata(model, forecastDistance = forecastDistance)
  status <- getMetadataStatus(metadata, backtest, source)
  if (!is.null(status) && !is.na(status) && status == DatetimeTrendPlotsStatuses$NotCompleted) {
    jobId <- ComputeDatetimeTrendPlots(
      model,
      backtest = backtest,
      source = source,
      forecastDistanceStart = forecastDistance,
      forecastDistanceEnd = forecastDistance
    )
    WaitForJobToComplete(model$projectId, jobId, maxWait = maxWait)
  }
}

#' Retrieve Accuracy over Time plot for a model.
#'
#' @inheritParams GetAccuracyOverTimePlotPreview
#' @param resolution character. Optional. Specifying at which resolution the data should be binned.
#'   If not provided an optimal resolution will be used to build chart data
#'   with number of \code{bins <= maxBinSize}. One of \code{DatetimeTrendPlotsResolutions}.
#' @param maxBinSize integer. Optional. An int between 1 and 1000, which specifies
#'   the maximum number of bins for the retrieval. Default is 500.
#' @param startDate POSIXct. Optional. The start of the date range to return.
#'   If not specified, start date for requested plot will be used.
#' @param endDate POSIXct. Optional. The end of the date range to return.
#'   If not specified, end date for requested plot will be used.
#' @return list with the following components:
#' \itemize{
#'   \item resolution. character: The resolution that is used for binning.
#'         One of \code{DatetimeTrendPlotsResolutions}.
#'   \item startDate. POSIXct: The datetime of the start of the chartdata (inclusive).
#'   \item endDate. POSIXct: The datetime of the end of the chartdata (exclusive).
#'   \item bins. data.frame: Each row represents a bin in the plot. Dataframe has following columns:
#'   \itemize{
#'     \item startDate. POSIXct: The datetime of the start of the bin (inclusive).
#'     \item endDate. POSIXct: The datetime of the end of the bin (exclusive).
#'     \item actual. numeric: Average actual value of the target in the bin.
#'           NA if there are no entries in the bin.
#'     \item predicted. numeric: Average prediction of the model in the bin.
#'           NA if there are no entries in the bin.
#'     \item frequency. integer: Indicates number of values averaged in bin.
#'   }
#'   \item statistics. list: Contains statistical properties for the plot.
#'   \itemize{
#'     \item durbinWatson. numeric: The Durbin-Watson statistic for the chart data.
#'           Value is between 0 and 4. Durbin-Watson statistic
#'           is a test statistic used to detect the presence of
#'           autocorrelation at lag 1 in the residuals (prediction errors)
#'           from a regression analysis.
#'   }
#'   \item calendarEvents. data.frame: Each row represents a calendar event in the plot.
#'         Dataframe has following columns:
#'   \itemize{
#'     \item date. POSIXct: The date of the calendar event.
#'     \item seriesId. character: The series ID for the event.
#'           If this event does not specify a series ID,
#'           then this will be NA, indicating that the event applies to all series.
#'     \item name. character: The name of the calendar event.
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' plot <- GetAccuracyOverTimePlot(model)
#'
#' # The code below is an example demonstration on how to plot the data
#' png("accuracy_over_time.png", width = 1200, height = 600, units = "px")
#' par(mar = c(10, 5, 5, 5))
#' plot(plot$bins$startDate, plot$bins$actual, type = "l", ylab = "Target", xaxt = "n", xlab = "")
#' lines(plot$bins$startDate, plot$bins$predicted, col = "red")
#' axis(1, plot$bins$startDate, format(plot$bins$startDate, "%Y-%m-%d"), las = 3)
#' title(xlab = "Date", mgp = c(7, 1, 0))
#' legend("topright", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = 1:1)
#' dev.off()
#' }
#' @export
GetAccuracyOverTimePlot <- function(model, backtest = 0,
                                    source = SourceType$Validation,
                                    seriesId = NULL, forecastDistance = NULL,
                                    maxBinSize = NULL, resolution = NULL,
                                    startDate = NULL, endDate = NULL,
                                    maxWait = 600) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId

  if (maxWait) {
    computeAccuracyOverTimePlotIfNotComputed(
      validModel, backtest, source, forecastDistance, maxWait
    )
  }

  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels", modelId, "accuracyOverTimePlots"
  )
  query <- list(
    seriesId = seriesId,
    backtest = backtest,
    source = source,
    resolution = resolution,
    forecastDistance = forecastDistance,
    maxBinSize = maxBinSize
  )
  if (!is.null(startDate)) {
    query$startDate <- formatRFC3339Timestamp(startDate)
  }
  if (!is.null(endDate)) {
    query$endDate <- formatRFC3339Timestamp(endDate)
  }

  query <- Filter(Negate(is.null), query)
  response <- DataRobotGET(routeString, query = query, simplifyDataFrame = TRUE)
  return(as.datetimeTrendPlot(response))
}

as.datetimeTrendPlot <- function(inList, preview = FALSE) {
  outList <- inList
  outList$startDate <- ParseRFC3339Timestamp(outList$startDate)
  outList$endDate <- ParseRFC3339Timestamp(outList$endDate)
  outList$bins$startDate <- do.call(ParseRFC3339Timestamp, list(outList$bins$startDate))
  outList$bins$endDate <- do.call(ParseRFC3339Timestamp, list(outList$bins$endDate))
  if (!preview && length(outList$calendarEvents)) {
    outList$calendarEvents$date <- do.call(
      ParseRFC3339Timestamp, list(outList$calendarEvents$date)
    )
  }
  return(outList)
}

#' Retrieve Accuracy over Time preview plot for a model.
#'
#' @inheritParams GetForecastVsActualPlotPreview
#' @param forecastDistance integer. Optional. Forecast distance to retrieve the chartdata for.
#'   If not specified, the first forecast distance for this project will be used.
#'   Only available for time series projects.
#' @return list with the following components:
#' \itemize{
#'   \item startDate. POSIXct: The datetime of the start of the chartdata (inclusive).
#'   \item endDate. POSIXct: The datetime of the end of the chartdata (exclusive).
#'   \item bins. data.frame: Each row represents a bin in the plot. Dataframe has following columns:
#'   \itemize{
#'     \item startDate. POSIXct: The datetime of the start of the bin (inclusive).
#'     \item endDate. POSIXct: The datetime of the end of the bin (exclusive).
#'     \item actual. numeric: Average actual value of the target in the bin.
#'           NA if there are no entries in the bin.
#'     \item predicted. numeric: Average prediction of the model in the bin.
#'           NA if there are no entries in the bin.
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' plot <- GetAccuracyOverTimePlotPreview(model)
#'
#' # The code below is an example demonstration on how to plot the data
#' png("accuracy_over_time_preview.png", width = 1200, height = 600, units = "px")
#' par(mar = c(10, 5, 5, 5))
#' plot(plot$bins$startDate, plot$bins$actual, type = "l", ylab = "Target", xaxt = "n", xlab = "")
#' lines(plot$bins$startDate, plot$bins$predicted, col = "red")
#' axis(1, plot$bins$startDate, format(plot$bins$startDate, "%Y-%m-%d"), las = 3)
#' title(xlab = "Date", mgp = c(7, 1, 0))
#' legend("topright", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = 1:1)
#' dev.off()
#' }
#' @export
GetAccuracyOverTimePlotPreview <- function(model, backtest = 0,
                                           source = SourceType$Validation,
                                           seriesId = NULL, forecastDistance = NULL,
                                           maxWait = 600) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId

  if (maxWait) {
    computeAccuracyOverTimePlotIfNotComputed(
      validModel, backtest, source, forecastDistance, maxWait
    )
  }

  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels", modelId,
    "accuracyOverTimePlots", "preview"
  )
  query <- list(
    seriesId = seriesId,
    backtest = backtest,
    source = source,
    forecastDistance = forecastDistance
  )
  query <- Filter(Negate(is.null), query)
  response <- DataRobotGET(routeString, query = query, simplifyDataFrame = TRUE)
  return(as.datetimeTrendPlot(response, preview = TRUE))
}

#' Retrieve Forecast vs Actual plots metadata for a model.
#'
#' @inheritParams DeleteModel
#' @return list with the following components:
#' \itemize{
#'   \item resolutions. list: A list of \code{DatetimeTrendPlotsResolutions},
#'         which represents available time resolutions for which plots can be retrieved.
#'   \item backtestStatuses. data.frame: Each row represents a list of forecast distances
#'         for status for the backtest \code{SourceType}.
#'         The row index corresponds to the backtest index via the relation
#'         \code{rowIndex <- backtestIndex + 1}. Status should be one of
#'         \code{DatetimeTrendPlotsStatuses}. If there are no forecast distances for some status
#'         it would be missing from the dataframe.
#'   \item backtestMetadata. data.frame: Each row represents a metadata for the backtest
#'         \code{SourceType} start and end date. The row index corresponds to the
#'         backtest index via the relation \code{rowIndex <- backtestIndex + 1}.
#'         Each cell contains a POSIXct timestamp for start date (inclusive)
#'         and end date (exclusive) if the corresponding source type
#'         for the backtest is computed, and NA otherwise.
#'   \item holdoutStatuses. list: Contains statuses for holdout.
#'   \itemize{
#'     \item training. list: list with the statuses, one of \code{DatetimeTrendPlotsStatuses}, and
#'           corresponding forecast distances for each status.
#'     \item validation. list: list with the statuses, one of \code{DatetimeTrendPlotsStatuses}, and
#'           corresponding forecast distances for each status.
#'   }
#'   \item holdoutMetadata. list. Contains metadata for holdout.
#'   \itemize{
#'     \item training. list. Contains start and end date for holdout training.
#'     \item validation. list. Contains start and end date for holdout validation.
#'       \itemize{
#'         \item startDate. POSIXct or NA:
#'               The datetime of the start of the holdout training/validation (inclusive).
#'               NA if the data is not computed.
#'         \item endDate. POSIXct or NA:
#'               The datetime of the end of the holdout training/validation (exclusive).
#'               NA if the data is not computed.
#'         }
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' GetForecastVsActualPlotsMetadata(model)
#' }
#' @export
GetForecastVsActualPlotsMetadata <- function(model) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels",
    modelId, "forecastVsActualPlots", "metadata"
  )
  response <- DataRobotGET(routeString, simplifyDataFrame = TRUE)
  return(as.datetimeTrendPlotsBacktestHoldoutMetadata(response))
}

computeForecastVsActualPlotIfNotComputed <- function(model, backtest, source,
                                                     forecastDistanceStart, forecastDistanceEnd,
                                                     maxWait) {
  metadata <- GetForecastVsActualPlotsMetadata(model)
  status <- getMetadataStatus(metadata, backtest, source)
  if (is.null(status) || (is.data.frame(status) &&
    DatetimeTrendPlotsStatuses$NotCompleted %notin% colnames(status)
  )) {
    return(NULL)
  }
  for (forecastDistance in unlist(status[[DatetimeTrendPlotsStatuses$NotCompleted]])) {
    if ((is.null(forecastDistanceStart) || forecastDistance >= forecastDistanceStart) &&
      (is.null(forecastDistanceEnd) || forecastDistance <= forecastDistanceEnd)) {
      jobId <- ComputeDatetimeTrendPlots(
        model,
        backtest = backtest,
        source = source,
        forecastDistanceStart = forecastDistanceStart,
        forecastDistanceEnd = forecastDistanceEnd
      )
      WaitForJobToComplete(model$projectId, jobId, maxWait = maxWait)
      break
    }
  }
}

#' Retrieve Forecast vs Actual plot for a model.
#'
#' @inheritParams DeleteModel
#' @inheritParams GetForecastVsActualPlotPreview
#' @param resolution character. Optional. Specifying at which resolution the data should be binned.
#'   If not provided an optimal resolution will be used to build chart data
#'   with number of \code{bins <= maxBinSize}. One of \code{DatetimeTrendPlotsResolutions}.
#' @param maxBinSize integer. Optional. An int between 1 and 1000, which specifies
#'   the maximum number of bins for the retrieval. Default is 500.
#' @param startDate POSIXct. Optional. The start of the date range to return.
#'   If not specified, start date for requested plot will be used.
#' @param endDate POSIXct. Optional. The end of the date range to return.
#'   If not specified, end date for requested plot will be used.
#' @param forecastDistanceStart integer. Optional. The start of forecast distance range
#'   (forecast window) to retrieve. If not specified, the first forecast distance
#'   for this project will be used.
#' @param forecastDistanceEnd integer. Optional. The end of forecast distance range
#'   (forecast window) to retrieve. If not specified, the last forecast distance
#'   for this project will be used.
#' @return list with the following components:
#' \itemize{
#'   \item resolution. character: The resolution that is used for binning.
#'         One of \code{DatetimeTrendPlotsResolutions}.
#'   \item startDate. POSIXct: The datetime of the start of the chartdata (inclusive).
#'   \item endDate. POSIXct: The datetime of the end of the chartdata (exclusive).
#'   \item forecastDistances. list: A list of forecast distances that were retrieved.
#'   \item bins. data.frame: Each row represents a bin in the plot. Dataframe has following columns:
#'   \itemize{
#'     \item startDate. POSIXct: The datetime of the start of the bin (inclusive).
#'     \item endDate. POSIXct: The datetime of the end of the bin (exclusive).
#'     \item actual. numeric: Average actual value of the target in the bin.
#'           NA if there are no entries in the bin.
#'     \item forecasts. list: Average forecasts of the model in the bin.
#'           Empty if there are no entries in the bin.
#'           Each index in the \code{forecasts} list maps to \code{forecastDistances} list index.
#'     \item error. numeric: Average absolute residual value of the bin.
#'           NA if there are no entries in the bin.
#'     \item normalizedError. numeric: Average normalized absolute residual value of the bin.
#'           NA if there are no entries in the bin.
#'     \item frequency. integer: Indicates number of values averaged in bin.
#'   }
#'   \item calendarEvents. data.frame: Each row represents a calendar event in the plot.
#'         Dataframe has following columns:
#'   \itemize{
#'     \item date. POSIXct: The date of the calendar event.
#'     \item seriesId. character: The series ID for the event.
#'           If this event does not specify a series ID,
#'           then this will be NA, indicating that the event applies to all series.
#'     \item name. character: The name of the calendar event.
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' plot <- GetForecastVsActualPlot(model)
#'
#' # The code below is an example demonstration on how to plot the data
#' forecastPointIndex <- 10
#' forecasts <- append(rep(NA, forecastPointIndex), plot$bins$forecasts[[forecastPointIndex]])
#' forecasts <- append(forecasts, rep(NA, length(plot$bins$forecasts) - length(forecasts)))
#' png("forecast_vs_actual.png", width = 1200, height = 600, units = "px")
#' par(mar = c(10, 5, 5, 5))
#' plot(plot$bins$startDate, plot$bins$actual, type = "l", ylab = "Target", xaxt = "n", xlab = "")
#' lines(plot$bins$startDate, forecasts, col = "red")
#' abline(v = plot$bins$startDate[[forecastPointIndex]], col = "blue")
#' axis(1, plot$bins$startDate, format(plot$bins$startDate, "%Y-%m-%d"), las = 3)
#' title(xlab = "Date", mgp = c(7, 1, 0))
#' legendEntries <- c("Actual", "Forecast", "Forecast Point")
#' legend("topright", legend = legendEntries, col = c("black", "red", "blue"), lty = 1:1)
#' dev.off()
#' }
#' @export
GetForecastVsActualPlot <- function(model, backtest = 0,
                                    source = SourceType$Validation,
                                    seriesId = NULL, forecastDistanceStart = NULL,
                                    forecastDistanceEnd = NULL, maxBinSize = NULL,
                                    resolution = NULL, startDate = NULL,
                                    endDate = NULL, maxWait = 600) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId

  if (maxWait) {
    computeForecastVsActualPlotIfNotComputed(
      validModel, backtest, source, forecastDistanceStart, forecastDistanceEnd, maxWait
    )
  }

  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels", modelId, "forecastVsActualPlots"
  )
  query <- list(
    seriesId = seriesId,
    backtest = backtest,
    source = source,
    resolution = resolution,
    forecastDistanceStart = forecastDistanceStart,
    forecastDistanceEnd = forecastDistanceEnd,
    maxBinSize = maxBinSize
  )
  if (!is.null(startDate)) {
    query$startDate <- formatRFC3339Timestamp(startDate)
  }
  if (!is.null(endDate)) {
    query$endDate <- formatRFC3339Timestamp(endDate)
  }

  query <- Filter(Negate(is.null), query)
  response <- DataRobotGET(routeString, query = query, simplifyDataFrame = TRUE)
  return(as.datetimeTrendPlot(response))
}

#' Retrieve Forecast vs Actual preview plot for a model.
#'
#' @inheritParams DeleteModel
#' @param backtest integer or character. Optional. Retrieve plots for a specific backtest.
#'   Use the backtest index starting from zero.
#'   To retrieve plots for holdout, use \code{DataSubset$Holdout}.
#' @param source character. Optional. The source of the data for the backtest/holdout.
#'   Must be one of \code{SourceType}.
#' @param seriesId character. Optional. The name of the series to retrieve for multiseries projects.
#'   If not provided an average plot for the first 1000 series will be retrieved.
#' @param maxWait integer. Optional. The maximum time to wait for a compute job to complete
#'   before retrieving the plots. Default is 600. If 0, the plots would be retrieved
#'   without attempting the computation.
#' @return list with the following components:
#' \itemize{
#'   \item startDate. POSIXct: The datetime of the start of the chartdata (inclusive).
#'   \item endDate. POSIXct: The datetime of the end of the chartdata (exclusive).
#'   \item bins. data.frame: Each row represents a bin in the plot. Dataframe has following columns:
#'   \itemize{
#'     \item startDate. POSIXct: The datetime of the start of the bin (inclusive).
#'     \item endDate. POSIXct: The datetime of the end of the bin (exclusive).
#'     \item actual. numeric: Average actual value of the target in the bin.
#'           NA if there are no entries in the bin.
#'     \item predicted. numeric: Average prediction of the model in the bin.
#'           NA if there are no entries in the bin.
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' plot <- GetForecastVsActualPlotPreview(model)
#'
#' # The code below is an example demonstration on how to plot the data
#' png("forecast_vs_actual_preview.png", width = 1200, height = 600, units = "px")
#' par(mar = c(10, 5, 5, 5))
#' plot(plot$bins$startDate, plot$bins$actual, type = "l", ylab = "Target", xaxt = "n", xlab = "")
#' lines(plot$bins$startDate, plot$bins$predicted, col = "red")
#' axis(1, plot$bins$startDate, format(plot$bins$startDate, "%Y-%m-%d"), las = 3)
#' title(xlab = "Date", mgp = c(7, 1, 0))
#' legend("topright", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = 1:1)
#' dev.off()
#' }
#' @export
GetForecastVsActualPlotPreview <- function(model, backtest = 0,
                                           source = SourceType$Validation,
                                           seriesId = NULL, maxWait = 600) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId

  if (maxWait) {
    computeForecastVsActualPlotIfNotComputed(
      validModel, backtest, source, NULL, NULL, maxWait
    )
  }

  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels", modelId,
    "forecastVsActualPlots", "preview"
  )
  query <- list(seriesId = seriesId, backtest = backtest, source = source)
  query <- Filter(Negate(is.null), query)
  response <- DataRobotGET(routeString, query = query, simplifyDataFrame = TRUE)
  return(as.datetimeTrendPlot(response, preview = TRUE))
}

#' Retrieve Anomaly over Time plots metadata for a model.
#'
#' @inheritParams DeleteModel
#' @return list with the following components:
#' \itemize{
#'   \item resolutions. list: A list of \code{DatetimeTrendPlotsResolutions},
#'         which represents available time resolutions for which plots can be retrieved.
#'   \item backtestStatuses. data.frame: Each row represents a status for the backtest
#'         \code{SourceType}. The row index corresponds to the backtest index via the relation
#'         \code{rowIndex <- backtestIndex + 1}. Status should be one of
#'         \code{DatetimeTrendPlotsStatuses}
#'   \item backtestMetadata. data.frame: Each row represents a metadata for the backtest
#'         \code{SourceType} start and end date. The row index corresponds to the
#'         backtest index via the relation \code{rowIndex <- backtestIndex + 1}.
#'         Each cell contains a POSIXct timestamp for start date (inclusive)
#'         and end date (exclusive) if the correspoding source type
#'         for the backtest is computed, and NA otherwise.
#'   \item holdoutStatuses. list: Contains statuses for holdout.
#'   \itemize{
#'     \item training. character: Status, one of \code{DatetimeTrendPlotsStatuses}
#'     \item validation. character: Status, one of \code{DatetimeTrendPlotsStatuses}
#'   }
#'   \item holdoutMetadata. list. Contains metadata for holdout.
#'   \itemize{
#'     \item training. list. Contains start and end date for holdout training.
#'     \item validation. list. Contains start and end date for holdout validation.
#'       \itemize{
#'         \item startDate. POSIXct or NA:
#'               The datetime of the start of the holdout training/validation (inclusive).
#'               NA if the data is not computed.
#'         \item endDate. POSIXct or NA:
#'               The datetime of the end of the holdout training/validation (exclusive).
#'               NA if the data is not computed.
#'         }
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' GetAnomalyOverTimePlotsMetadata(model)
#' }
#' @export
GetAnomalyOverTimePlotsMetadata <- function(model) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels",
    modelId, "anomalyOverTimePlots", "metadata"
  )
  response <- DataRobotGET(routeString, simplifyDataFrame = TRUE)
  return(as.datetimeTrendPlotsBacktestHoldoutMetadata(response))
}

computeAnomalyOverTimePlotIfNotComputed <- function(model, backtest, source, maxWait) {
  metadata <- GetAnomalyOverTimePlotsMetadata(model)
  status <- getMetadataStatus(metadata, backtest, source)
  if (!is.null(status) && !is.na(status) && status == DatetimeTrendPlotsStatuses$NotCompleted) {
    jobId <- ComputeDatetimeTrendPlots(model, backtest = backtest, source = source)
    WaitForJobToComplete(model$projectId, jobId, maxWait = maxWait)
  }
}

#' Retrieve Anomaly over Time plot for a model.
#'
#' @inheritParams GetForecastVsActualPlotPreview
#' @param resolution character. Optional. Specifying at which resolution the data should be binned.
#'   If not provided an optimal resolution will be used to build chart data
#'   with number of \code{bins <= maxBinSize}. One of \code{DatetimeTrendPlotsResolutions}.
#' @param maxBinSize integer. Optional. An int between 1 and 1000, which specifies
#'   the maximum number of bins for the retrieval. Default is 500.
#' @param startDate POSIXct. Optional. The start of the date range to return.
#'   If not specified, start date for requested plot will be used.
#' @param endDate POSIXct. Optional. The end of the date range to return.
#'   If not specified, end date for requested plot will be used.
#' @return list with the following components:
#' \itemize{
#'   \item resolution. character: The resolution that is used for binning.
#'         One of \code{DatetimeTrendPlotsResolutions}.
#'   \item startDate. POSIXct: The datetime of the start of the chartdata (inclusive).
#'   \item endDate. POSIXct: The datetime of the end of the chartdata (exclusive).
#'   \item bins. data.frame: Each row represents a bin in the plot. Dataframe has following columns:
#'   \itemize{
#'     \item startDate. POSIXct: The datetime of the start of the bin (inclusive).
#'     \item endDate. POSIXct: The datetime of the end of the bin (exclusive).
#'     \item predicted. numeric: Average prediction of the model in the bin.
#'           NA if there are no entries in the bin.
#'     \item frequency. integer: Indicates number of values averaged in bin.
#'   }
#'   \item calendarEvents. data.frame: Each row represents a calendar event in the plot.
#'         Dataframe has following columns:
#'   \itemize{
#'     \item date. POSIXct: The date of the calendar event.
#'     \item seriesId. character: The series ID for the event.
#'           If this event does not specify a series ID,
#'           then this will be NA, indicating that the event applies to all series.
#'     \item name. character: The name of the calendar event.
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' plot <- GetAnomalyOverTimePlot(model)
#'
#' # The code below is an example demonstration on how to plot the data
#' png("anomaly_over_time.png", width = 1200, height = 600, units = "px")
#' par(mar = c(10, 5, 5, 5))
#' ylab <- "Anomaly score"
#' plot(plot$bins$startDate, plot$bins$predicted, type = "l", ylab = ylab, xaxt = "n", xlab = "")
#' axis(1, plot$bins$startDate, format(plot$bins$startDate, "%Y-%m-%d"), las = 3)
#' title(xlab = "Date", mgp = c(7, 1, 0))
#' legend("topright", legend = c("Predicted"), col = c("black"), lty = 1:1)
#' dev.off()
#' }
#' @export
GetAnomalyOverTimePlot <- function(model, backtest = 0,
                                   source = SourceType$Validation,
                                   seriesId = NULL, maxBinSize = NULL,
                                   resolution = NULL, startDate = NULL,
                                   endDate = NULL, maxWait = 600) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId

  if (maxWait) {
    computeAnomalyOverTimePlotIfNotComputed(validModel, backtest, source, maxWait)
  }

  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels", modelId, "anomalyOverTimePlots"
  )
  query <- list(
    seriesId = seriesId,
    backtest = backtest,
    source = source,
    resolution = resolution,
    maxBinSize = maxBinSize
  )
  if (!is.null(startDate)) {
    query$startDate <- formatRFC3339Timestamp(startDate)
  }
  if (!is.null(endDate)) {
    query$endDate <- formatRFC3339Timestamp(endDate)
  }

  query <- Filter(Negate(is.null), query)
  response <- DataRobotGET(routeString, query = query, simplifyDataFrame = TRUE)
  return(as.datetimeTrendPlot(response))
}

#' Retrieve Anomaly over Time preview plot for a model.
#'
#' @inheritParams GetForecastVsActualPlotPreview
#' @param predictionThreshold numeric. Optional. Only bins with predictions exceeding
#'   this threshold will be returned in the response.
#' @return list with the following components:
#' \itemize{
#'   \item startDate. POSIXct: The datetime of the start of the chartdata (inclusive).
#'   \item endDate. POSIXct: The datetime of the end of the chartdata (exclusive).
#'   \item predictionThreshold. numeric: Only bins with predictions exceeding
#'         this threshold are returned in the response.
#'   \item bins. data.frame: Each row represents a bin in the plot. Dataframe has following columns:
#'   \itemize{
#'     \item startDate. POSIXct: The datetime of the start of the bin (inclusive).
#'     \item endDate. POSIXct: The datetime of the end of the bin (exclusive).
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' plot <- GetAnomalyOverTimePlotPreview(model, predictionThreshold = 0.1)
#'
#' # The code below is an example demonstration on how to plot the data
#' x <- seq(plot$startDate, plot$endDate, plot$bins$endDate[1] - plot$bins$startDate[1])
#' png("anomaly_over_time_preview.png", width = 1200, height = 600, units = "px")
#' par(mar = c(10, 5, 5, 5))
#' plot(x, rep(0, length(x)), type = "l", yaxt = "n", ylab = "", xaxt = "n", xlab = "")
#' points(plot$bins$startDate, rep(0, length(plot$bins$startDate)), pch = 19, col = "red")
#' axis(1, x, format(x, "%Y-%m-%d"), las = 3)
#' title(xlab = "Date", mgp = c(7, 1, 0))
#' legendString <- sprintf("Anomaly for prediction threshold %.2f", plot$predictionThreshold)
#' legend("topright", legend = c(legendString), col = c("red"), pch = c(19))
#' dev.off()
#' }
#' @export
GetAnomalyOverTimePlotPreview <- function(model, predictionThreshold = 0.5, backtest = 0,
                                          source = SourceType$Validation, seriesId = NULL,
                                          maxWait = 600) {
  validModel <- ValidateAndReturnModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId

  if (maxWait) {
    computeAnomalyOverTimePlotIfNotComputed(validModel, backtest, source, maxWait)
  }

  routeString <- UrlJoin(
    "projects", projectId, "datetimeModels", modelId,
    "anomalyOverTimePlots", "preview"
  )
  query <- list(
    predictionThreshold = predictionThreshold,
    seriesId = seriesId,
    backtest = backtest,
    source = source
  )
  query <- Filter(Negate(is.null), query)
  response <- DataRobotGET(routeString, query = query, simplifyDataFrame = TRUE)
  return(as.datetimeTrendPlot(response, preview = TRUE))
}
