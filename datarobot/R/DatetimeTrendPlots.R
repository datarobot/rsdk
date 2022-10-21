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
ComputeDatetimeTrendPlots <- function(model, backtest = 0,
                                      source = SourceType$Validation, forecastDistanceStart = NULL,
                                      forecastDistanceEnd = NULL) {
    validModel <- ValidateModel(model)
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
GetAccuracyOverTimePlotsMetadata <- function(model, forecastDistance = NULL) {
    validModel <- ValidateModel(model)
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
        return(parseRFC3339Timestamp(timestamp))
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
#' @inheritParams DeleteModel
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
#' GetAccuracyOverTimePlot(model)
#' plot <- GetAccuracyOverTimePlot(model)
#' png("accuracy_over_time.png", width = 1200, height = 600, units = "px")
#' par(mar = c(10, 5, 5, 5))
#' plot(plot$bins$startDate, plot$bins$actual, type = "l", ylab = "Target", xaxt = "n", xlab = "")
#' lines(plot$bins$startDate, plot$bins$predicted, col = "red")
#' axis(1, plot$bins$startDate, format(plot$bins$startDate, "%Y-%m-%d"), las = 3)
#' title(xlab = "Date", mgp = c(7, 1, 0))
#' legend("topright", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = 1:1)
#' dev.off()
#' }
GetAccuracyOverTimePlot <- function(model, backtest = 0,
                                    source = SourceType$Validation,
                                    seriesId = NULL, forecastDistance = NULL,
                                    maxBinSize = NULL, resolution = NULL,
                                    startDate = NULL, endDate = NULL,
                                    maxWait = 600) {
    validModel <- ValidateModel(model)
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
    if (!is.null(startDate)) { query$startDate <- formatRFC3339Timestamp(startDate) }
    if (!is.null(endDate)) { query$endDate <- formatRFC3339Timestamp(endDate) }

    query <- Filter(Negate(is.null), query)
    response <- DataRobotGET(routeString, query = query, simplifyDataFrame = TRUE)
    return(as.datetimeTrendPlot(response))
}

as.datetimeTrendPlot <- function(inList, preview = FALSE) {
    outList <- inList
    outList$startDate <- parseRFC3339Timestamp(outList$startDate)
    outList$endDate <- parseRFC3339Timestamp(outList$endDate)
    outList$bins$startDate <- do.call(parseRFC3339Timestamp, list(outList$bins$startDate))
    outList$bins$endDate <- do.call(parseRFC3339Timestamp, list(outList$bins$endDate))
    if (!preview && length(outList$calendarEvents)) {
        outList$calendarEvents$date <- do.call(
            parseRFC3339Timestamp, list(outList$calendarEvents$date)
        )
    }
    return(outList)
}

#' Retrieve Accuracy over Time preview plot for a model.
#'
#' @inheritParams DeleteModel
#' @param backtest integer or character. Optional. Retrieve plots for a specific backtest.
#'   Use the backtest index starting from zero.
#'   To retrieve plots for holdout, use \code{DataSubset$Holdout}.
#' @param source character. Optional. The source of the data for the backtest/holdout.
#'   Must be one of \code{SourceType}.
#' @param seriesId character. Optional. The name of the series to retrieve for multiseries projects.
#'   If not provided an average plot for the first 1000 series will be retrieved.
#' @param forecastDistance integer. Optional. Forecast distance to retrieve the chartdata for.
#'   If not specified, the first forecast distance for this project will be used.
#'   Only available for time series projects.
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
#' plot <- GetAccuracyOverTimePlotPreview(model)
#' png("accuracy_over_time_preview.png", width = 1200, height = 600, units = "px")
#' par(mar = c(10, 5, 5, 5))
#' plot(plot$bins$startDate, plot$bins$actual, type = "l", ylab = "Target", xaxt = "n", xlab = "")
#' lines(plot$bins$startDate, plot$bins$predicted, col = "red")
#' axis(1, plot$bins$startDate, format(plot$bins$startDate, "%Y-%m-%d"), las = 3)
#' title(xlab = "Date", mgp = c(7, 1, 0))
#' legend("topright", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = 1:1)
#' dev.off()
#' }
GetAccuracyOverTimePlotPreview <- function(model, backtest = 0,
                                           source = SourceType$Validation,
                                           seriesId = NULL, forecastDistance = NULL,
                                           maxWait = 600) {
    validModel <- ValidateModel(model)
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
