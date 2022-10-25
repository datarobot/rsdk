# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Download the time series feature derivation log as a text file.
#'
#' @inheritParams DeleteProject
#' @param file character. The name or path of the file to download to.
#' @seealso \code{\link{GetTimeSeriesFeatureDerivationLog}}
#' @return Nothing, but writes the output to the desired file.
#' @examples
#' \dontrun{
#' projectId <- "5984b4d7100d2b31c1166529"
#' DownloadTimeSeriesFeatureDerivationLog(projectId, "featureLog.txt")
#' }
#' @export
DownloadTimeSeriesFeatureDerivationLog <- function(project, file) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "timeSeriesFeatureLog", "file")
  response <- DataRobotGET(routeString, as = "file", filename = file)
  invisible(NULL)
}
