DefaultHTTPTimeout <- 60

#' Make a HTTP request
#'
#' @param requestMethod function. A function from httr (e.g., `httr::GET`, `httr::POST`) to use.
#' @param routeString character. The path to make the request on.
#' @param addUrl logical. Should the endpoint be prepended to the routeString? (Default TRUE).
#' @param returnRawResponse logical. Whether to return the raw httr response object (as opposed
#'   to post processing and returning the content of that object, which is the default.)
#' @param as character. What should the resulting data be interpreted as? (default "json").
#'   Use "file" to download as a file (see \code{filename}).
#' @param simplifyDataFrame logical. Whether to invoke \code{jsonlite::simplifyDataFrame}.
#' @param body list. The body of the request for POST.
#' @param query list. The query parameters for GET.
#' @param timeout numeric. How many seconds before the request times out?
#' @param encode character. What should the body be encoded as for the JSON request?
#' @param followLocation logical. Should HTTR follow the location if provided? (Default TRUE).
#' @param filename character. The path of the file to download to, if it is a download request.
#' @param stopOnError logical. If there is an error, should it be raised as a fatal R error?
#'  (Default TRUE).
MakeDataRobotRequest <- function(requestMethod,
                                 routeString,
                                 addUrl = TRUE,
                                 returnRawResponse = TRUE,
                                 as = "json",
                                 simplifyDataFrame = TRUE,
                                 body = NULL,
                                 query = NULL,
                                 timeout = DefaultHTTPTimeout,
                                 encode = NULL,
                                 followLocation = TRUE,
                                 filename = NULL,
                                 stopOnError = TRUE) {
  # Makes authenticated DataRobot requests. Most uses require the $content element from the return
  # list, but some require the raw response, which is returned if returnRawResponse is TRUE
  path <- BuildPath(routeString, addUrl)
  SetSSLVerification()

  # ...Okay, deep breath. If we want to pass NULL values into the API we're going to need to do
  # some heavy work, as httr will drop NULL values by default.
  if (!is.null(body) && TryingToSubmitNull(body)) { # detect if any NULL values are present and
                                                    # switch to alternate handling...
    # To properly pass null values as JSON, we need to get the parameter value to be `null`.
    # Interpreting to JSON gives `{}` instead of `null`, so we hack this with find-and-replace.
    body <- structure(gsub("{}", "null",
                           as.character(jsonlite::toJSON(body)), fixed = TRUE),
                      class = "json")
    # To pass raw JSON into the request, we have to encode with "raw" instead of "json"
    encode <- "raw"
    # We also have to manually set the header to JSON
    headers <- DataRobotAddHeaders(Authorization = path$authHead,
                                   "Content-Type" = "application/json")
  } else {
    headers <- DataRobotAddHeaders(Authorization = path$authHead)
  }

  # Construct arguments for request method
  args <- list(url = path$fullPath,
               config = headers,
               httr::timeout(timeout))
  args$body <- body
  args$query <- query
  args$encode <- encode
  if (isTRUE(getOption("dataRobotCurlDebugMode"))) {
    # If a debug mode is activated, use httr::verbose to print the internals of the
    # API call for debugging purposes.
    args <- c(args, list(httr::verbose()))
  }
  if (identical(followLocation, FALSE)) {
    args <- c(args, list(httr::config(followlocation = 0)))
  }
  if (identical(as, "file")) {
    args <- c(args, list(httr::write_disk(filename, overwrite = TRUE)))
  }

  # Run the call to request method
  rawResponse <- do.call(requestMethod, args)
  if (ResponseHasDeprecationHeader(rawResponse)) {
    # DSX-2384 warn about deprecated/disabled resources that have the Deprecation header set
    DeprecatedHeaderMessage(rawResponse)
  }

  # Return response
  if (isTRUE(stopOnError)) {
    StopIfResponseIsError(rawResponse)
  }
  if (isTRUE(returnRawResponse) || identical(as, "file")) {
    rawResponse
  } else if (identical(as, "json")) {
    ParseReturnResponse(rawResponse, simplifyDataFrame = simplifyDataFrame)
  } else {
    httr::content(rawResponse, as = as, encoding = "UTF-8")
  }
}

DataRobotGET <- function(routeString,
                         addUrl = TRUE,
                         returnRawResponse = FALSE, ...) {
  MakeDataRobotRequest(httr::GET, routeString,
                       addUrl = addUrl,
                       returnRawResponse = returnRawResponse,
                       ...)
}

DataRobotDELETE <- function(routeString,
                            addUrl = TRUE,
                            returnRawResponse = FALSE, ...) {
  MakeDataRobotRequest(httr::DELETE,
                       routeString,
                       addUrl = addUrl,
                       returnRawResponse = returnRawResponse,
                       ...)
}

DataRobotPATCH <- function(routeString,
                           addUrl = TRUE,
                           body = NULL,
                           returnRawResponse = FALSE, ...) {
  MakeDataRobotRequest(httr::PATCH, routeString,
                       addUrl = addUrl,
                       returnRawResponse = returnRawResponse,
                       body = body,
                       ...)
}

DataRobotPOST <- function(routeString,
                          addUrl = TRUE,
                          body = NULL,
                          returnRawResponse = FALSE, ...) {
  MakeDataRobotRequest(httr::POST, routeString,
                       addUrl = addUrl,
                       returnRawResponse = returnRawResponse,
                       simplifyDataFrame = TRUE,
                       body = body,
                       ...)
}


DataRobotGetDefaultHeader <- function() {
  platform <- as.list(Sys.info())
  platformStr <- paste(platform$sysname, platform$release, platform$machine)
  userAgent <- sprintf("DataRobotRClient/%s (%s)",
                       GetClientVersion(),
                       platformStr)
  suffix <- UserAgentSuffix()
  if (nzchar(suffix)) {
    userAgent <- paste(userAgent, suffix)
  }
  userAgent
}

DataRobotAddHeaders <- function(...) {
  httr::add_headers("User-Agent" = DataRobotGetDefaultHeader(), ...)
}


StopIfResponseIsError <- function(rawReturn) {
  if (is.null(rawReturn)) { # Error if the entire HTTR object is missing... probably only happens
                            # with a bad test mock but want to be sure.
    stop("No HTTR response object was returned.")
  }
  returnStatus <- httr::status_code(rawReturn)
  if (returnStatus >= 400) {
    parsedReturn <- ParseReturnResponse(rawReturn)
    errors <- parsedReturn$errors
    errorSummary <- parsedReturn$message
    errorMessages <- Map(function(keyName) paste0("  ", keyName, ": ", errors[[keyName]]),
                         names(errors))
    errorMessagesCombined <- paste(errorMessages, collapse = "\n")
    statusString <- sprintf("(%s)", returnStatus)
    stop(paste(statusString, errorSummary, errorMessagesCombined, sep = "\n"), call. = FALSE)
  }
  TRUE
}


Endpoint <- function() {
  Sys.getenv("DATAROBOT_API_ENDPOINT")
}

Token <- function() {
  Sys.getenv("DATAROBOT_API_TOKEN")
}

UserAgentSuffix <- function() {
  Sys.getenv("DataRobot_User_Agent_Suffix")
}

SSLVerify <- function() {
  Sys.getenv("DataRobot_SSL_Verify")
}


BuildPath <- function(routeString, addUrl = TRUE) {
  endpoint <- Endpoint()
  token <- Token()
  if (endpoint == "" | token == "") {
    rawMsg <- paste("User authentication required. See ConnectToDataRobot documentation")
    stop(strwrap(rawMsg), call. = FALSE)
  } else if (isTRUE(addUrl)) {
    fullPath <- UrlJoin(endpoint, routeString)
  } else {
    fullPath <- routeString
  }
  CheckUrl(fullPath)
  authHead <- paste("Token", token)
  list(fullPath = fullPath, authHead = authHead)
}


#' Make sure the path is a reasonable URL
#'
#' @param url character. The URL to check.
CheckUrl <- function(url) {
  if (grepl("//$", url) || grepl("\\s", url) || !grepl("/", url)) {
    stop(paste("Internal error, URL invalid:", url))
  }
  TRUE
}


UrlJoin <- function(...) {
  for (urlComponent in list(...)) {
    if (grepl("\\?", urlComponent)) {
      stop("Arguments to UrlJoin should not contain query parameters.")
    }
  }
  components <- list(...)
  # Remove trailing /'s to avoid double slashing when pasting
  components <- Map(function(x) sub("\\/+$", "", x), components)
  components <- c(components, "") # for trailing "/"
  paste(components, collapse = "/")
}


ParseReturnResponse <- function(rawReturn, ...) {
  OnError <- function(error) {
    stop(paste("Expected JSON, received:\n", textContent),
         call. = FALSE)
  }

  textContent <- httr::content(rawReturn, as = "text", encoding = "UTF-8")

  if (is.na(textContent)) {
    NA
  } else if (identical(textContent, "")) {
    ""
  } else {
    tryCatch(jsonlite::fromJSON(textContent, ...), error = OnError)
  }
}


#' Checks to see if we are trying to submit `NULL` as a value.
#' @param body list. The body to check for NULL.
TryingToSubmitNull <- function(body) {
  if (is.list(body)) { any(unlist(lapply(body, TryingToSubmitNull))) }
  else if (is.null(body)) { TRUE }
  else { FALSE }
}


ResponseIsRedirection <- function(rawResponse) {
  responseCategory <- httr::http_status(rawResponse)$category
  tolower(responseCategory) == "redirection"
}

ResponseHasDeprecationHeader <- function(rawResponse) {
  tryCatch(
    {
      headers <- httr::headers(rawResponse)
      if (is.null(headers) || is.null(headers$Deprecation)) {
        return(FALSE)
      }
      return(TRUE)
    },
    error = function(e) {
      # This code really only catches errors with poor test mocks that
      # don't set up a full httr:::response properly
      return(FALSE)
    }
  )
}
