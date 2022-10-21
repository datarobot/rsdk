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

#' parseRFC3339Timestamp
#'
#' The DataRobot APIs returns dates in RFC 3339 format.
#'
#' @param timestampstring character. Timestamp in RFC 3339 format.
#' @returns The input timestamp as a POSIXt
#' @family API datetime functions
parseRFC3339Timestamp <- function(timestampstring) {
    return(as.POSIXct(timestampstring,
                      format = RFC3339DateTimeFormat,
                      tz = "UTC"))
}
