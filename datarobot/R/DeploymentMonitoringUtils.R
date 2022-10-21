#' Helper function for validating reporting period objects used by
#' the deployment monitoring functions. See \code{GetDeploymentServiceStats},
#' \code{GetDeploymentAccuracy}, \code{GetDeploymentServiceStatsOverTime}, and
#' \code{GetDeploymentAccuracyOverTime}.
#'
#' @param timestamp character. A timestamp in RFC 3339 format.
#' @param tsName character. Optional. Explanation of the timestamp for error messages.
#' @family API datetime functions
validateReportingPeriodTime <- function(timestamp, tsName = "timestamp") {
    timestamp <- as.POSIXlt(timestamp)
    if (any(
        timestamp$min != 0,
        timestamp$sec != 0
    )) {
        stop(paste(tsName, "does not support sub-hour precision"))
    }
    if (any(timestamp$tzone != "UTC", timestamp$gmtoff != 0)) {
        stop(paste(tsName, "must be in UTC timezone"))
    }
    TRUE
}

#' The DataRobot Monitoring APIs return dates formatted as RFC 3339
#' strings. This is the same as ISO 8601. Specifically, 'T' is the
#' date/time separator and 'Z' is used to denote UTC. Fractional seconds
#' are returned. e.g. 2020-01-01T05:00:00.000000Z
#'
#' @param periodContainer an object containing the following:
#' \itemize{
#'   \item period list, containing the following two items:
#'   \itemize{
#'      \item start character. RFC 3339 formatted timestamp.
#'      \item end character. RFC 3339 formatted timestamp.
#'   }
#' }
#' @family API datetime functions
transformRFC3339Period <- function(periodContainer) {
    if (!is.list(periodContainer$period) && is.na(periodContainer$period)) {
        periodContainer$period <- list()
        periodContainer$period$start <- NA
        periodContainer$period$end <- NA
    }
    periodContainer$period <- ApplySchema(periodContainer$period, c("start", "end"))
    periodContainer$period$start <- parseRFC3339Timestamp(periodContainer$period$start)
    periodContainer$period$end <- parseRFC3339Timestamp(periodContainer$period$end)
    periodContainer
}

#' Tidies a ServiceOverTime response object for use in a DF
#' @param df A data frame that contains the following:
#' \itemize{
#'   \item period list, containing the following two items:
#'   \itemize{
#'      \item start POSIXct.
#'      \item end POSIXct.
#'   }
#'   \item value object.
#' }
#' @param valueColName character. The column in df currently named 'value' will be renamed to this.
tidyServiceOverTimeObject <- function(df, valueColName) {
    df <- jsonlite::flatten(as.data.frame(df))
    names(df)[names(df) == "period.start"] <- "start"
    names(df)[names(df) == "period.end"] <- "end"
    names(df)[names(df) == "value"] <- valueColName
    df
}
