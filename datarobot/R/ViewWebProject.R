# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Retrieve a DataRobot web page that displays detailed project information
#'
#' This function brings up a web page that displays detailed project
#' information like that available from the standard DataRobot user interface.
#'
#' @param project object. Either list with projectId element or projectId value
#'
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' ViewWebProject(projectId)
#' }
#' @export
ViewWebProject <- function(project) {
  projectId <- ValidateProject(project)
  dataRobotUrl <- Sys.getenv("DATAROBOT_API_ENDPOINT")
  parsedUrl <- httr::parse_url(dataRobotUrl)
  urlString <- MakeUrl(parsedUrl, projectId = projectId)
  DataRobotBrowse(urlString)
  if (is.list(project)) {
    projectName <- project$projectName
  } else {
    projectName <- NULL
  }
  if (is.null(projectName)) {
    message(paste("Opened URL", urlString, "for selected project"))
  } else {
    message(paste("Opened URL", urlString, "for project:", projectName))
  }
}
