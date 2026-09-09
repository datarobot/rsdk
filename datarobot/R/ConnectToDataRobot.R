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
#' variables), except where noted below for Certificate Authority (CA) bundle handling.
#' If you have a YAML config file or environment variables set, you will not need to
#' pass any parameters to \code{ConnectToDataRobot} in order to connect.
#'
#' A custom Certificate Authority (CA) bundle can be supplied in three equivalent ways:
#' \itemize{
#'   \item Pass the path directly via the \code{caBundle} argument.
#'   \item Set \code{ca_bundle: /path/to/my.pem} in your \code{drconfig.yaml}.
#'   \item Export the \env{CURL_CA_BUNDLE} environment variable before starting R.
#' }
#' When both \code{configPath} and \code{caBundle} are supplied, the configuration file's
#' \code{ca_bundle} entry takes precedence; the environment variable is consulted only when
#' neither of the other surfaces provides a bundle. Unlike the other connection parameters,
#' the CA bundle allows this layering instead of rejecting mixed inputs outright.
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
#' @param caBundle character. Path to a PEM-encoded Certificate Authority (CA) bundle file used
#'   to verify SSL/TLS connections. Useful when connecting to a DataRobot instance that uses a
#'   private or self-signed CA. When provided, this value is stored in the
#'   \env{CURL_CA_BUNDLE} environment variable and applied to all subsequent requests in
#'   the session. If \code{NULL} (the default), the value of \env{CURL_CA_BUNDLE} already
#'   present in the environment (if any) is preserved and used. Can also be set via
#'   \code{ca_bundle} in \code{drconfig.yaml}.
#' @param username character. No longer supported.
#' @param password character. No longer supported.
#' @examples
#' \dontrun{
#'   ConnectToDataRobot("https://app.datarobot.com/api/v2", "thisismyfaketoken")
#'   ConnectToDataRobot(configPath = "~/.config/datarobot/drconfig.yaml")
#'   # Connect using a private CA bundle
#'   ConnectToDataRobot(
#'     endpoint = "https://app.datarobot.com/api/v2",
#'     token = "thisismyfaketoken",
#'     caBundle = "/path/to/my-ca-bundle.pem"
#'   )
#' }
#' @export
ConnectToDataRobot <- function(endpoint = NULL,
                               token = NULL,
                               username = NULL,
                               password = NULL,
                               userAgentSuffix = NULL,
                               sslVerify = TRUE,
                               configPath = NULL,
                               caBundle = NULL
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
  SaveCABundlePreference(caBundle)
  if (numAuthMethodsProvided > 1) {
    stop("Please provide only one of: config file or token.")
  } else if (haveToken) {
    ConnectWithToken(endpoint, token)
  } else if (haveUsernamePassword) {
    ConnectWithUsernamePassword(endpoint, username, password)
  } else if (haveConfigPath) {
    ConnectWithConfigFile(configPath)
  } else if (!is.na(envEndpoint) && !is.na(envToken)) {
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
                     sslVerify = config$ssl_verify, caBundle = config$ca_bundle)
}

#' Configure SSL verification and optional CA bundle for httr.
#'
#' Applies the session preferences stored in \env{DataRobot_SSL_Verify} and
#' \env{CURL_CA_BUNDLE}. Verification stays enabled by default; disabling it via
#' \code{sslVerify = FALSE} turns off both peer and host checks. When a CA bundle path is
#' present it is validated and fed to libcurl via \code{cainfo}.
SetSSLVerification <- function() {
  sslVerify <- Sys.getenv("DataRobot_SSL_Verify")
  if (identical(sslVerify, "FALSE")) {
    httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
    return(invisible(NULL))
  }

  caBundle <- Sys.getenv("CURL_CA_BUNDLE", unset = NA_character_)
  configArgs <- list(
    ssl_verifypeer = 1L,
    ssl_verifyhost = 2L
  )

  if (!is.na(caBundle) && nzchar(caBundle)) {
    expandedBundle <- path.expand(caBundle)
    if (!file.exists(expandedBundle)) {
      stop(
        sprintf("CURL_CA_BUNDLE is set to '%s' but that file does not exist.", caBundle),
        call. = FALSE
      )
    }
    configArgs$cainfo <- tryCatch(
      normalizePath(expandedBundle, winslash = "/", mustWork = TRUE),
      error = function(e) {
        stop(
          sprintf("CURL_CA_BUNDLE is set to '%s' but that file does not exist.", caBundle),
          call. = FALSE
        )
      }
    )
  }

  httr::set_config(do.call(httr::config, configArgs))
  invisible(NULL)
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
  packageStartupMessage("Authentication token saved")
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

#' Save the CA bundle path to the session environment variable.
#'
#' When \code{caBundle} is non-NULL, validates that it is a single character string pointing
#' to an existing file, then writes it to \env{CURL_CA_BUNDLE}. When NULL, the function
#' is a no-op, preserving any pre-existing value of \env{CURL_CA_BUNDLE} (which allows
#' users to set the variable before starting R and have it picked up automatically).
#'
#' @param caBundle character or NULL. Path to a PEM-encoded CA bundle file.
#' @keywords internal
SaveCABundlePreference <- function(caBundle) {
  if (!is.null(caBundle)) {
    if (length(caBundle) != 1 || !is.character(caBundle)) {
      stop("caBundle must be a single character string (a file path) or NULL.")
    }
    expandedBundle <- path.expand(caBundle)
    if (!file.exists(expandedBundle)) {
      stop(sprintf("caBundle file not found: %s", caBundle))
    }
    resolvedBundle <- tryCatch(
      normalizePath(expandedBundle, winslash = "/", mustWork = TRUE),
      error = function(e) {
        stop(sprintf("caBundle file not found: %s", caBundle), call. = FALSE)
      }
    )
    Sys.setenv(CURL_CA_BUNDLE = resolvedBundle)
  }
  # NULL: intentionally a no-op -- preserve any pre-existing CURL_CA_BUNDLE env var.
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
