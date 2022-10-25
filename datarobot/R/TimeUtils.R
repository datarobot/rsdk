# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' RFC 3339 datetime format
#'
#' The DataRobot API returns dates in RFC 3339 format. Since this comes from a
#' Python datetime object, we assume that the period returned is in the format
#' "%Y-%m-%dT%H:%M:%OSZ".
#' @family API datetime functions
RFC3339DateTimeFormat <- "%Y-%m-%dT%H:%M:%OSZ"


#' formatRFC3339Timestamp
#'
#' The DataRobot APIs expect dates formatted as RFC 3339 strings. This is the
#' same as ISO 8601. To be safe, use UTC as the timezone (and format it with a
#' 'Z' suffix), and use 'T' as the date/time separator.
#' @param date POSIXt or date. The date(s) to be formatted.
#' @family API datetime functions
formatRFC3339Timestamp <- function(date) {
  date <- as.POSIXct(date, tz = "UTC")
  if (requireNamespace("lubridate", quietly = TRUE)) {
    dateString <- lubridate::format_ISO8601(date, usetz = TRUE)
  } else {
    dateString <- format(date, RFC3339DateTimeFormat, tz = "UTC")
  }
  return(dateString)
}
