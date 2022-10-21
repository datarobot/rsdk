#' Retrieve the time series feature derivation log content
#'
#' The time series feature derivation log provides details about the feature generation process
#' for a time series project. It includes information about which features are generated and their
#' priority, as well as the detected properties of the time series data such as whether the
#' series is stationary, and periodicities detected.
#'
#' This route is only supported for time series projects that have finished partitioning.
#' The time series feature log will include information about:
#' \itemize{
#'   \item Detected stationarity of the series (e.g. "Series detected as non-stationary")
#'   \item Detected presence of multiplicative trend in the series (e.g., "Multiplicative
#'     trend detected")
#'   \item Any periodicities (e.g., "Detected periodicities: 7 day")
#'   \item Maximum number of feature to be generated (e.g., "Maximum number of feature to be
#'     generated is 1440")
#'   \item Window sizes used in rolling statistics / lag extractors (e.g., "The window sizes
#'     chosen to be: 2 months") (because the time step is 1 month and Feature Derivation
#'     Window is 2 months)
#'   \item Features that are specified as known-in-advance (e.g., "Variables treated as
#'     known in advance: holiday")
#'   \item Details about why certain variables are transformed in the input data (e.g.,
#'     "Generating variable "y (log)" from "y" because multiplicative trend is detected")
#'   \item Details about features generated as time series features, and their priority
#'     (e.g., "Generating feature "date (actual)" from "date" (priority: 1)")
#' }
#' @inheritParams DeleteProject
#' @param offset integer. Optional. Default is 0. This many results will be skipped.
#' @param limit integer. Optional. Defaults to 100. At most this many results are returned.
#'   To specify no limit, use 0. The default may change without notice.
#' @return Returns the feature log output
#' @examples
#' \dontrun{
#'  projectId <- "5984b4d7100d2b31c1166529"
#'  GetTimeSeriesFeatureDerivationLog(projectId)
#' }
#' @export
GetTimeSeriesFeatureDerivationLog <- function(project, offset = NULL, limit = NULL) {
  body <- list()
  if (!is.null(offset)) { body$offset <- offset }
  if (!is.null(limit)) { body$limit <- limit }
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "timeSeriesFeatureLog")
  featureData <- DataRobotGET(routeString, body = body)
  featureData$featureLog
}


#' Download the time series feature derivation log as a text file.
#'
#' @inheritParams DeleteProject
#' @param file character. The name or path of the file to download to.
#' @seealso \code{\link{GetTimeSeriesFeatureDerivationLog}}
#' @return Nothing, but writes the output to the desired file.
#' @examples
#' \dontrun{
#'  projectId <- "5984b4d7100d2b31c1166529"
#'  DownloadTimeSeriesFeatureDerivationLog(projectId, "featureLog.txt")
#' }
#' @export
DownloadTimeSeriesFeatureDerivationLog <- function(project, file) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "timeSeriesFeatureLog", "file")
  response <- DataRobotGET(routeString, as = "file", filename = file)
  invisible(NULL)
}
