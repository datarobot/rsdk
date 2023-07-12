Deprecated <- function(message, deprecatedInVersion, removedInVersion) {
  .Deprecated(msg = sprintf(
    "%s has been deprecated in %s, will be removed in %s.",
    message, deprecatedInVersion, removedInVersion
  ))
}

#' @keywords internal
Py2DeprecationUrl <- function() {
  prefix <- "/api/v2"
  dataRobotUrl <- strsplit(Endpoint(), prefix)[[1]]
  return(paste0(dataRobotUrl, "/docs/release/deprecations-and-migrations/python2.html"))
}

#' Handler for Deprecated header in API responses
#'
#' @param rawResponse An httr response object.
#' @keywords internal
DeprecatedHeaderMessage <- function(rawResponse) {
  # This assumes that the mere existence of a Deprecation: true header in a response from the
  # DataRobot Public API indicates a resource has been deprecated. We will use the string
  # returned from the server in the header unless that string is either empty of "true" in
  # which case we use a default message.
  # It does not use .Deprecated() as this marker is specific to deprecated R code.

  headers <- httr::headers(rawResponse)
  headerDeprecationMessage <- headers$Deprecation

  defaultWarningMsg <- paste(
    sep = "\"",
    "The resource you are trying to access will be or is deprecated. For additional guidance, run `browseURL(",
    Py2DeprecationUrl(),
    ")` or login to the DataRobot app for this project."
  )

  if (tolower(headerDeprecationMessage) == "true" || headerDeprecationMessage == "") {
    warningMessage <- defaultWarningMsg
  } else {
    warningMessage <- headerDeprecationMessage
  }

  warning(
    strwrap(
      warningMessage,
      prefix = " ", initial = ""
    ),
    call. = FALSE
  )
}
