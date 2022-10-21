#' Retrieve accuracy statistics for a deployment.
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
#' @param segmentAttribute character. Optional. The name of an attribute used for segment analysis.
#' See `SegmentAnalysisAttribute` for permitted values. Added in DataRobot 2.21.
#' @param segmentValue character. Optional. The value of \code{segmentAttribute}. Added in DataRobot
#'   2.21.
#' @param targetClasses character. Optional. List of target classes to filter out of the response.
#'   Added in DataRobot 2.23.
#' @return An object representing service health metrics for the deployment, containing:
#' \itemize{
#'   \item modelId character. The ID of the deployment model for which monitoring data was
#'     retrieved.
#'   \item period list. The duration of the reporting period, containing:
#'     \itemize{
#'       \item start POSIXct. Start of the reporting period.
#'       \item end POSIXct. End of the reporting period.
#'     }
#'   \item metrics data.frame. Accuracy metrics for the deployment, where each row is a separate
#'     metric and contains the columns:
#'     \itemize{
#'       \item metric. character. Name of the metric. See \code{DeploymentAccuracyMetric} for valid
#'         values.
#'       \item baselineValue. numeric. May be NA if accuracy data is not available.
#'       \item value. numeric. May be NA if accuracy data is not available.
#'       \item percentChange. numeric. The percent change of value over baseline. May be NA if
#'         accuracy data is not available.
#'     }
#'   \item segmentAttribute character. Optional. The name of the segment on which segment analysis
#'     was performed. Added in DataRobot 2.21.
#'   \item segmentValue character. Optional. The value of the segmentAttribute. Added in DataRobot
#'     2.21.
#' }
#' @examples
#' \dontrun{
#' library(dplyr)
#' deploymentId <- "59a5af20c80891534e3c2bde"
#' acc <- GetDeploymentAccuracy(deploymentId, end = ISOdate(2021, 01, 06, 1, 0, 0, tz = "UTC"))
#' df <- mutate(
#'        acc$metrics,
#'        "modelId" = acc$modelId,
#'        "startTime" = acc$period$start,
#'        "endTime" = acc$period$end,
#'        .before = everything()
#'    )
#' }
#' @family deployment accuracy functions
#' @md
#' @export
GetDeploymentAccuracy <- function(deploymentId,
                                  modelId = NULL,
                                  start = NULL,
                                  end = NULL,
                                  segmentAttribute = NULL,
                                  segmentValue = NULL,
                                  targetClasses = NULL) {
    routeString <- UrlJoin("deployments", deploymentId, "accuracy")
    query <- list()
    query$modelId <- modelId
    if (!is.null(start) && validateReportingPeriodTime(start, "start")) {
        query$start <- formatRFC3339Timestamp(start)
    }
    if (!is.null(end) && validateReportingPeriodTime(end, "end")) {
        query$end <- formatRFC3339Timestamp(end)
    }
    query$segmentAttribute <- segmentAttribute
    query$segmentValue <- segmentValue
    query$targetClasses <- targetClasses
    response <- DataRobotGET(routeString, query = query)
    as.dataRobotDeploymentAccuracy(response)
}

as.dataRobotDeploymentAccuracy <- function(inlist) {
    # The API response structures accuracy measurements as a list of lists.
    # This function transforms that structure into a data.frame.
    outlist <- transformRFC3339Period(inlist)
    outlist$metrics <- transformNestedListToDF(outlist$metrics)
    class(outlist) <- c("deploymentAccuracy")
    outlist
}

#' Retrieves accuracy statistics over time on given metrics for a deployment.
#'
#' By default this will return statistics for the last seven days prior to the next; set the `start`
#' and `end` parameters to adjust the reporting period.
#'
#' @param deploymentId character. The ID of the deployment in question.
#' @param metrics character. Metrics to query. See `DeploymentAccuracyMetric` for supported
#'   values.
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
#' @param segmentAttribute character. Optional. The name of an attribute used for segment analysis.
#'   See SegmentAnalysisAttribute`` for permitted values. Added in DataRobot 2.21.
#' @param segmentValue character. Optional. The value of `segmentAttribute`. Added in DataRobot
#'   2.21.
#' @return An object representing how accuracy has changed over time for the deployment, containing:
#' \itemize{
#'   \item modelId character. The ID of the deployment model for which monitoring data was
#'     retrieved.
#'   \item summary data.frame. A summary `bucket` across the entire reporting period.
#'   \item buckets data.frame. A list of `buckets` representing each interval (constrained by
#'     the `bucketSize` parameter) in the reporting period.
#'   \item baseline data.frame. A baseline `bucket`.
#' }
#' Each `bucket` contains:
#' \itemize{
#'   \item sampleSize. integer. The number of predictions made against this deployment.
#'   \item start. POSIXct. The start time of the bucket. May be NA.
#'   \item end. POSIXct. The end time of the bucket. May be NA.
#'   \item `metricName`. numeric. Given N metrics queried, there will be N value columns, each
#'     one named for the metric. See `DeploymentAccuracyMetric` for supported values. May be NA if
#'     `sampleSize` is 0.
#' }
#' @examples
#' \dontrun{
#' deploymentId <- "59a5af20c80891534e3c2bde"
#' aot <- GetDeploymentAccuracyOverTime(deploymentId,
#'          metrics = c(DeploymentAccuracyMetric$Gamma.Deviance,
#'                      DeploymentAccuracyMetric$LogLoss,
#'                      DeploymentAccuracyMetric$RMSE))
#' }
#' @family deployment accuracy functions
#' @md
#' @export
GetDeploymentAccuracyOverTime <- function(deploymentId,
                                          metrics,
                                          modelId = NULL,
                                          start = NULL,
                                          end = NULL,
                                          bucketSize = NULL,
                                          segmentAttribute = NULL,
                                          segmentValue = NULL) {
    if (length(metrics) == 0) {
        warning("Metrics should not be an empty vector")
    }
    invalidMetrics <- setdiff(metrics, DeploymentAccuracyMetric)
    if (length(invalidMetrics) > 0) {
        warning(paste0(c("These metrics are not valid: ", sQuote(invalidMetrics))))
    }
    # Remove invalid metrics from API invocation
    metrics <- setdiff(metrics, invalidMetrics)
    responses <- lapply(metrics, function(m)
        getDeploymentAccuracyOverTimeSingleMetric(
            deploymentId,
            metric = m,
            modelId,
            start,
            end,
            bucketSize,
            segmentAttribute,
            segmentValue
        )
    )
    as.dataRobotDeploymentAccuracyOverTimeDF(responses)
}

getDeploymentAccuracyOverTimeSingleMetric <- function(deploymentId,
                                                      metric = NULL,
                                                      modelId = NULL,
                                                      start = NULL,
                                                      end = NULL,
                                                      bucketSize = NULL,
                                                      segmentAttribute = NULL,
                                                      segmentValue = NULL) {
    routeString <- UrlJoin("deployments", deploymentId, "accuracyOverTime")
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
    query$segmentAttribute <- segmentAttribute
    query$segmentValue <- segmentValue

    response <- DataRobotGET(routeString, query = query)
    as.dataRobotDeploymentAccuracyOverTime(response)
}

as.dataRobotDeploymentAccuracyOverTimeDF <- function(inlist) {
    Reduce(function(a, b) {
        a$summary <- merge(a$summary, b$summary)
        a$buckets <- merge(a$buckets, b$buckets)
        a$baseline <- merge(a$baseline, b$baseline)
        a
    }, inlist)
}

as.dataRobotDeploymentAccuracyOverTime <- function(inlist) {
    outlist <- inlist

    # Retrieve this metric name since we'll be using it for a colname,
    # then discard it from the response
    metricName <- outlist$metric
    outlist$metric <- NULL

    outlist$summary <- nullToNA(outlist$summary)
    outlist$summary <- transformRFC3339Period(outlist$summary)
    outlist$summary <- tidyServiceOverTimeObject(outlist$summary, metricName)
    if (is.na(outlist$summary$sampleSize)) {
        outlist$summary$sampleSize <- 0L
    }

    outlist$buckets <- nullToNA(outlist$buckets)
    outlist$buckets <- transformRFC3339Period(outlist$buckets)
    outlist$buckets <- tidyServiceOverTimeObject(outlist$buckets, metricName)

    outlist$baseline <- nullToNA(outlist$baseline)
    outlist$baseline <- transformRFC3339Period(outlist$baseline)
    outlist$baseline <- tidyServiceOverTimeObject(outlist$baseline, metricName)
    if (is.na(outlist$baseline$sampleSize)) {
        outlist$baseline$sampleSize <- 0L
    }

    class(outlist) <- "deploymentAccuracyOverTime"
    outlist
}

transformNestedListToDF <- function(inlist) {
    # API will return 'null' for accuracy metrics that DNE,
    # as when actuals have not been submitted, so convert
    # these to NA
    metrics <- lapply(lapply(inlist, rbind), nullToNA)
    metrics <- do.call(rbind, metrics)
    # in the output, set first column = metric name (from the input list names)
    outlist <- matrix(names(inlist))
    colnames(outlist) <- "metric"
    # combine
    outlist <- cbind(outlist, metrics)
    data.frame(outlist)
}
