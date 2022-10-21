#' Retrieve service health statistics for a deployment.
#'
#' @param deploymentId character. The ID of the deployment.
#' @param modelId character. Optional. The ID of the model to query. If provided, only data for this
#'   specific model will be retrieved; otherwise, data for the deployment's default model will be
#'   retrieved.
#' @param start POSIXct. Optional. The start time of the reporting period for monitoring data.
#'   Defaults to seven days prior to the end of the period. Sub-hour resolution is not permitted,
#'   and the timezone must be `UTC`.
#' @param end POSIXct. Optional. The end time of the reporting period for monitoring data. Defaults
#'   to the next top of the hour. Sub-hour resolution is not permitted, and the timezone must be
#'   `UTC`.
#' @param executionTimeQuantile numeric. Optional. Quantile for the `executionTime` metric. Defaults
#'   to 0.5.
#' @param responseTimeQuantile numeric. Optional. Quantile for the `responseTime` metric. Defaults
#'   to 0.5.
#' @param slowRequestsThreshold integer. Optional. Threshold for the `slowRequests` metric.
#'   Defaults to 1000.
#' @param segmentAttribute character. Optional. The name of an attribute used for segment analysis.
#'   See `SegmentAnalysisAttribute` for permitted values. Added in DataRobot 2.20.
#' @param segmentValue character. Optional. The value of `segmentAttribute`. Added in DataRobot
#'   2.20.
#' @return An object representing service health metrics for the deployment, containing:
#' \itemize{
#'   \item modelId character. The ID of the deployment model for which monitoring data was
#'     retrieved.
#'   \item period list. The duration of the reporting period, containing:
#'     \itemize{
#'       \item start POSIXct. Start of the reporting period.
#'       \item end POSIXct. End of the reporting period.
#'     }
#'   \item metrics list. Service health metrics for the deployment, containing:
#'     \itemize{
#'       \item totalPredictions integer. Total number of prediction rows.
#'       \item totalRequests integer. Total number of prediction requests performed.
#'       \item slowRequests integer. Number of requests with response time greater than
#'         `slowRequestsThreshold`.
#'       \item responseTime numeric. Request response time at `responseTimeQuantile` in
#'         milliseconds. May be NA.
#'       \item executionTime numeric. Request execution time at `executionTimeQuantile` in
#'         milliseconds. May be NA.
#'       \item medianLoad integer. Median request rate, in requests per minute.
#'       \item peakLoad integer. Greatest request rate, in requests per minute.
#'       \item userErrorRate numeric. Ratio of user errors to the total number of requests.
#'       \item serverErrorRate numeric. Ratio of server errors to the total number of requests.
#'       \item numConsumers integer. Number of unique users performing requests.
#'       \item cacheHitRatio numeric. The ratio of cache hits to requests.
#'     }
#'   \item segmentAttribute character. Added in DataRobot 2.20. The name of the segment on which
#'     segment analysis was performed.
#'   \item segmentValue character. Added in DataRobot 2.20. The value of the segmentAttribute.
#' }
#' @examples
#' \dontrun{
#'   deploymentId <- "59a5af20c80891534e3c2bde"
#'   startTime = ISOdate(2020, 12, 25, 1, 0, 0, tz = "UTC")
#'   endTime = ISOdate(2021, 01, 06, 1, 0, 0, tz = "UTC")
#'   GetDeploymentServiceStats(deploymentId, startTime, endTime)
#' }
#' \dontrun{
#'   deploymentId <- "59a5af20c80891534e3c2bde"
#'   GetDeploymentServiceStats(deploymentId,
#'                             segmentAttribute = SegmentAnalysisAttribute$DataRobotRemoteIP,
#'                             segmentValue = "192.168.0.1")
#' }
#' @md
#' @export
GetDeploymentServiceStats <- function(deploymentId,
                                      modelId = NULL, start = NULL, end = NULL,
                                      executionTimeQuantile = NULL, responseTimeQuantile = NULL,
                                      slowRequestsThreshold = NULL, segmentAttribute = NULL,
                                      segmentValue = NULL
                                      ) {
    routeString <- UrlJoin("deployments", deploymentId, "serviceStats")
    query <- list()
    query$modelId <- modelId
    if (!is.null(start) && validateReportingPeriodTime(start, "start")) {
        query$start <- formatRFC3339Timestamp(start)
    }
    if (!is.null(end) && validateReportingPeriodTime(end, "end")) {
        query$end <- formatRFC3339Timestamp(end)
    }
    query$executionTimeQuantile <- executionTimeQuantile
    query$responseTimeQuantile <- responseTimeQuantile
    query$slowRequestsThreshold <- slowRequestsThreshold
    query$segmentAttribute <- segmentAttribute
    query$segmentValue <- segmentValue
    response <- DataRobotGET(routeString, query = query)
    as.dataRobotDeploymentServiceStats(response)
}

as.dataRobotDeploymentServiceStats <- function(inlist) {
    outlist <- inlist
    outlist <- transformRFC3339Period(outlist)
    outlist$metrics <- lapply(outlist$metrics, function(x) {
        ifelse(is.null(x), as.numeric(NA), x)
    })
    outlist$metrics$totalPredictions <- as.integer(outlist$metrics$totalPredictions)
    outlist$metrics$totalRequests <- as.integer(outlist$metrics$totalRequests)
    outlist$metrics$slowRequests <- as.integer(outlist$metrics$slowRequests)
    outlist$metrics$medianLoad <- as.integer(outlist$metrics$medianLoad)
    outlist$metrics$peakLoad <- as.integer(outlist$metrics$peakLoad)
    outlist$metrics$numConsumers <- as.integer(outlist$metrics$numConsumers)

    class(outlist) <- "deploymentServiceStats"
    outlist
}

#' Retrieves service health statistics over time on given metrics for a deployment.
#'
#' By default this will return statistics for the last seven days prior to the next; set the `start`
#' and `end` parameters to adjust the reporting period.
#'
#' @param deploymentId character. The ID of the deployment.
#' @param metrics character. Optional. Metrics to query. See `DeploymentServiceHealthMetric` for
#'   supported values. If not provided, defaults to `TotalPredictions`.
#' @param modelId character. Optional. The ID of the model to query. If provided, only data for this
#'   specific model will be retrieved; otherwise, data for the deployment's default model will be
#'   retrieved.
#' @param start POSIXct. Optional. The start time of the reporting period for monitoring data.
#'   Defaults to seven days prior to the end of the period. Sub-hour resolution is not permitted,
#'   and the timezone must be `UTC`.
#' @param end POSIXct. Optional. The end time of the reporting period for monitoring data. Defaults
#'   to the next top of the hour. Sub-hour resolution is not permitted, and the timezone must be
#'   `UTC`.
#' @param bucketSize character. Optional. The time duration of a bucket. This should be a multiple
#'   of one hour and cannot be longer than the total length of the period. If not set, a default
#'   value will be calculated based on the `start` and `end` times.
#' @param quantile numeric. Optional. Quantile for the `executionTime` and `responseTime` metrics.
#'   Defaults to 0.5.
#' @param threshold integer. Optional. Threshold for the `slowQueries` metric. Defaults to 1000.
#' @param segmentAttribute character. Optional. The name of an attribute used for segment analysis.
#'   See `SegmentAnalysisAttribute` for permitted values. Added in DataRobot 2.20.
#' @param segmentValue character. Optional. The value of `segmentAttribute`. Added in DataRobot
#'   2.20.
#' @return
#' \itemize{
#'   \item modelId character. The ID of the deployment model for which monitoring data was
#'     retrieved.
#'   \item summary data.frame. Summarizes statistics for each metric over the entire reporting
#'     period.
#'   \item buckets data.frame. Statistics for each metric, split into intervals of equal duration.
#'     There is one column representing stats for each metric queried, as well as:
#'     \itemize{
#'       \item start POSIXct. Start of the interval.
#'       \item end POSIXct. End of the interval.
#'     }
#'   \item segmentAttribute character. Added in DataRobot 2.20. The name of the segment on which
#'     segment analysis was performed.
#'   \item segmentValue character. Added in DataRobot 2.20. The value of `segmentAttribute`.
#' }
#' @examples
#' \dontrun{
#' metrics <- c(DeploymentServiceHealthMetric)
#' GetDeploymentServiceStatsOverTime(deploymentId, metrics = metrics)
#' }
#' @md
#' @export
GetDeploymentServiceStatsOverTime <- function(deploymentId,
                                              metrics = DeploymentServiceHealthMetric$TotalPredictions, # nolint
                                              modelId = NULL,
                                              start = NULL,
                                              end = NULL,
                                              bucketSize = NULL,
                                              quantile = NULL,
                                              threshold = NULL,
                                              segmentAttribute = NULL,
                                              segmentValue = NULL) {
    if (length(metrics) == 0) {
        warning("Metrics should not be an empty vector")
    }
    invalidMetrics <- setdiff(metrics, DeploymentServiceHealthMetric)
    if (length(invalidMetrics) > 0) {
        warning(paste0(c("These metrics are not valid: ", sQuote(invalidMetrics))))
    }
    # Remove invalid metrics from API invocation
    metrics <- setdiff(metrics, invalidMetrics)

    responses <- lapply(metrics, function(m) getDeploymentServiceStatsOverTimeSingleMetric(
        deploymentId,
        metric = m,
        modelId,
        start,
        end,
        bucketSize,
        quantile,
        threshold,
        segmentAttribute,
        segmentValue
    ))
    Reduce(function(a, b) {
        a$summary <- merge(a$summary, b$summary)
        a$buckets <- merge(a$buckets, b$buckets)
        a
    }, responses)
}

getDeploymentServiceStatsOverTimeSingleMetric <- function(deploymentId,
                                              metric = NULL,
                                              modelId = NULL,
                                              start = NULL,
                                              end = NULL,
                                              bucketSize = NULL,
                                              quantile = NULL,
                                              threshold = NULL,
                                              segmentAttribute = NULL,
                                              segmentValue = NULL) {
    routeString <- UrlJoin("deployments", deploymentId, "serviceStatsOverTime")
    query <- list()
    query$metric <- metric
    query$modelId <- modelId
    if (!is.null(start) && validateReportingPeriodTime(start, "start")) {
        query$start <- formatRFC3339Timestamp(start)
    }
    if (!is.null(end) && validateReportingPeriodTime(end, "end")) {
        query$end <- formatRFC3339Timestamp(end)
    }
    query$bucketSize <- bucketSize
    query$quantile <- quantile
    query$threshold <- threshold
    query$segmentAttribute <- segmentAttribute
    query$segmentValue <- segmentValue

    response <- DataRobotGET(routeString, query = query)
    as.dataRobotDeploymentServiceStatsOverTime(response, metric)
}

as.dataRobotDeploymentServiceStatsOverTime <- function(inlist, metricName) {
    outlist <- inlist

    outlist$summary <- transformRFC3339Period(outlist$summary)
    outlist$summary <- tidyServiceOverTimeObject(outlist$summary, metricName)

    outlist$buckets <- transformRFC3339Period(outlist$buckets)
    outlist$buckets <- tidyServiceOverTimeObject(outlist$buckets, metricName)

    class(outlist) <- "deploymentServiceStatsOverTime"
    outlist
}
