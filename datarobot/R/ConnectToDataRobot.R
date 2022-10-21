#' Establish a connection to the DataRobot modeling engine
#'
#' This function initializes a DataRobot session. To use DataRobot, you must connect to
#' your account. This can be done in three ways:
#' \itemize{
#'   \item by passing an \code{endpoint} and \code{token} directly to \code{ConnectToDataRobot}
#'   \item by having a YAML config file in $HOME/.config/datarobot/drconfig.yaml
#'   \item by setting DATAROBOT_API_ENDPOINT and DATAROBOT_API_TOKEN environment variables
#' }
#' The three methods of authentication are given priority in that order (explicitly passing
#' parameters to the function will trump a YAML config file, which will trump the environment
#' variables.)
#' If you have a YAML config file or environment variables set, you will not need to
#' pass any parameters to \code{ConnectToDataRobot} in order to connect.
#'
#' @param endpoint character. URL specifying the DataRobot server to be used.
#'   It depends on DataRobot modeling engine implementation (cloud-based, on-prem...) you are using.
#'   Contact your DataRobot admin for endpoint to use and to turn on API access to your account.
#'   The endpoint for DataRobot cloud accounts is https://app.datarobot.com/api/v2
#' @param token character. DataRobot API access token. It is unique for each DataRobot modeling
#'   engine account and can be accessed using DataRobot webapp in Account profile section.
#' @param userAgentSuffix character. Additional text that is appended to the
#'   User-Agent HTTP header when communicating with the DataRobot REST API. This
#'   can be useful for identifying different applications that are built on top
#'   of the DataRobot Python Client, which can aid debugging and help track
#'   usage.
#' @param sslVerify logical. Whether to check the SSL certificate. Either
#'   TRUE to check (default), FALSE to not check.
#' @param configPath character. Path to YAML config file specifying configuration
#'   (token and endpoint).
#' @param username character. No longer supported.
#' @param password character. No longer supported.
#' @examples
#' \dontrun{
#'   ConnectToDataRobot("https://app.datarobot.com/api/v2", "thisismyfaketoken")
#'   ConnectToDataRobot(configPath = "~/.config/datarobot/drconfig.yaml")
#' }
#' @export
ConnectToDataRobot <- function(endpoint = NULL,
                               token = NULL,
                               username = NULL,
                               password = NULL,
                               userAgentSuffix = NULL,
                               sslVerify = TRUE,
                               configPath = NULL
) {
  #  Check environment variables
  envEndpoint <- Sys.getenv("DATAROBOT_API_ENDPOINT", unset = NA)
  envToken <- Sys.getenv("DATAROBOT_API_TOKEN", unset = NA)

  #  If the user provides a token, save it to the environment
  #  variable DATAROBOT_API_TOKEN and call ListProjects to verify it

  haveToken <- !is.null(token)
  haveUsernamePassword <- (!is.null(username)) || (!is.null(password))
  haveConfigPath <- !is.null(configPath)
  numAuthMethodsProvided <- haveToken + haveConfigPath + haveUsernamePassword
  if (!is.null(userAgentSuffix)) {
    SaveUserAgentSuffix(userAgentSuffix)
  }
  SaveSSLVerifyPreference(sslVerify)
  if (numAuthMethodsProvided > 1) {
    stop("Please provide only one of: config file or token.")
  } else if (haveToken) {
    ConnectWithToken(endpoint, token)
  } else if (haveUsernamePassword) {
    ConnectWithUsernamePassword(endpoint, username, password)
  } else if (haveConfigPath) {
    ConnectWithConfigFile(configPath)
  } else if (!is.na(envEndpoint) & !is.na(envToken)) {
    ConnectWithToken(envEndpoint, envToken)
  } else {
    errorMsg <- "No authentication method provided."
    stop(strwrap(errorMsg), call. = FALSE)
  }
}

GetDefaultConfigPath <- function() {
  file.path(Sys.getenv("HOME"), ".config", "datarobot", "drconfig.yaml")
}

ConnectWithConfigFile <- function(configPath) {
  config <- yaml::yaml.load_file(configPath)
  # Since the options we get from the config come in snake_case, but ConnectToDataRobot()
  # wants camelCase arguments, we manually map the config options to their correct argument.
  # We _could_ do this programmatically, but with the small number of options we support,
  # it doesn't seem worth it.
  if (!is.null(config$ssl_verify) &&
      (length(config$ssl_verify) != 1 || !is.logical(config$ssl_verify))) {
    stop("ssl_verify must be either unset or set as either TRUE or FALSE.")
  }
  ConnectToDataRobot(endpoint = config$endpoint, token = config$token, username = config$username,
                     password = config$password, userAgentSuffix = config$user_agent_suffix,
                     sslVerify = config$ssl_verify)
}

SetSSLVerification <- function() {
  sslVerify <- Sys.getenv("DataRobot_SSL_Verify")
  if (identical(sslVerify, "FALSE")) {
    httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
  }
}

ConnectWithToken <- function(endpoint, token) {
  authHead <- paste("Token", token, sep = " ")
  subUrl <- paste("/", "projects/", sep = "")
  fullURL <- paste(endpoint, subUrl, sep = "")
  SetSSLVerification()
  rawReturn <- httr::GET(fullURL, DataRobotAddHeaders(Authorization = authHead))
  newURL <- gsub(subUrl, "", rawReturn$url)
  StopIfDenied(rawReturn)
  if (!grepl(endpoint, rawReturn$url, fixed = TRUE)) {
    errorMsg <- paste0("Specified endpoint ", endpoint, " is not correct.",
                       "\nWas redirected to ", newURL)
    stop(errorMsg, call. = FALSE)
  }
  out <- SaveConnectionEnvironmentVars(endpoint, token)
  VersionWarning()
  RStudioConnectionOpened(endpoint, token)
  invisible(out)
}

ConnectWithUsernamePassword <- function(endpoint, username, password) {
  stop("Using your username/password to authenticate with the DataRobot API is no longer supported.
       Please supply your API token instead. You can find your API token in your account profile in
       the DataRobot web app.")
}

SaveConnectionEnvironmentVars <- function(endpoint, token) {
  message("Authentication token saved")
  Sys.setenv(DATAROBOT_API_ENDPOINT = endpoint)
  Sys.setenv(DATAROBOT_API_TOKEN = token)
}

SaveUserAgentSuffix <- function(suffix) {
  Sys.setenv(DataRobot_User_Agent_Suffix = suffix)
}

SaveSSLVerifyPreference <- function(sslVerify) {
  if (!is.null(sslVerify)) {
    if (length(sslVerify) != 1 || !is.logical(sslVerify)) {
      stop("sslVerify must be unset or be TRUE or FALSE.")
    }
    Sys.setenv(DataRobot_SSL_Verify = sslVerify)
  }
}

StopIfDenied <- function(rawReturn) {
  returnStatus <- httr::status_code(rawReturn)
  if (returnStatus >= 400) {
    response <- unlist(ParseReturnResponse(rawReturn))
    errorMsg <- paste("Authorization request denied: ", response)
    stop(strwrap(errorMsg), call. = FALSE)
  }
}

VersionWarning <- function() {
  clientVer <- GetClientVersion()
  serverVer <- GetServerVersion()
  if (is.null(serverVer)) {
    invisible(NULL)
  }
  if (clientVer$major != serverVer$major) {
    errMsg <-
      paste("\n Client and server versions are incompatible. \n Server version: ",
            serverVer$versionString, "\n Client version: ", clientVer)
    stop(errMsg)
  }
  if (clientVer$minor > serverVer$minor) {
    warMsg <-
      paste("Client version is ahead of server version, you may have incompatibilities")
    warning(warMsg, call. = FALSE)
  }
}

GetServerVersion <- function() {
  dataRobotUrl <- Sys.getenv("DATAROBOT_API_ENDPOINT")
  errorMessage <-
    paste("Server did not reply with an API version. This may indicate the endpoint ", dataRobotUrl,
          "\n is misconfigured, or that the server API version precedes this version \n  ",
          "of the DataRobot client package and is likely incompatible.")
  ver <- tryCatch({routeString <- UrlJoin("version")
  modelInfo <- DataRobotGET(routeString)
  },
  ConfigError = function(e) {
    warning(errorMessage)
    ver <- NULL
  })
  ver
}

GetClientVersion <- function() {
  packageVersion("datarobot")
}
