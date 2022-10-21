#' Function to set up a new DataRobot project
#'
#' This function uploads a modeling dataset defined by the dataSource parameter
#' and allows specification of the optional project name projectName. The
#' dataSource parameter can be either the name of a CSV file or a dataframe;
#' in the latter case, it is saved as a CSV file whose name is described in
#' the Details section. This function returns the projectName specified in the
#' calling sequence, the unique alphanumeric identifier projectId for the new
#' project, the name of the modeling dataset uploaded to create this project,
#' and the project creation time and date.
#'
#' The DataRobot modeling engine requires a CSV file containing the data to be
#' used in fitting models, and this has been implemented here in two ways.
#' The first and simpler is to specify dataSource as the name of this CSV file,
#' but for the convenience of those who wish to work with dataframes, this
#' function also provides the option of specifying a dataframe, which is then
#' written to a CSV file and uploaded to the DataRobot server. In this case, the
#' file name is either specified directly by the user through the saveFile
#' parameter, or indirectly from the name of the dataSource dataframe if
#' saveFile = NULL (the default).  In this second case, the file name consists
#' of the name of the dataSource dataframe with the string csvExtension appended.
#'
#' @param dataSource object. Either (a) the name of a CSV file, (b) a dataframe
#'   or (c) url to a publicly available file; in each case, this parameter identifies
#'  the source of the data from which all project models will be built. See Details.
#' @param projectName character. Optional. String specifying a project name.
#' @param maxWait integer. The maximum time to wait for each of two steps: (1) The initial project
#'   creation request, and (2) data processing that occurs after receiving the response to this
#'   initial request.
#' @inherit as.dataRobotProjectShort return
#' @examples
#' \dontrun{
#'   SetupProject(iris, "dr-iris")
#' }
#' @export
SetupProject <- function(dataSource, projectName = NULL,
                         maxWait = 60 * 60) {
  if (is.null(projectName)) { projectName <- deparse(substitute(dataSource)) }
  dataList <- list(projectName = projectName)
  if (isURL(dataSource)) {
    dataList$url <- dataSource
  } else {
    dataList$file <- UploadData(dataSource)
  }
  routeString <- "projects/"
  postResponse <- DataRobotPOST(routeString, body = dataList,
                                returnRawResponse = TRUE, timeout = maxWait)
  message(paste("Project", projectName,
                "creation requested, awaiting creation"))
  project <- ProjectFromJobResponse(postResponse, maxWait = maxWait)
  message(sprintf("Project %s (%s) created", project$projectId, project$projectName))
  as.dataRobotProjectShort(project)
}


#' Function to set up a new DataRobot project using datasource on a WebHDFS server (deprecated)
#'
#' This function is deprecated. Use \code{SetupProjectFromDataSource} instead.
#'
#' This function returns the projectName specified in the
#' calling sequence, the unique alphanumeric identifier projectId for the new
#' project, the name of the modeling dataset uploaded to create this project,
#' and the project creation time and date.
#'
#' @param url character. The location of the WebHDFS file,
#'   both server and full path. Per the DataRobot specification, must begin with hdfs://
#' @param port integer. Optional. The port to use. If not specified, will default to the server
#'   default (50070).
#' @param projectName character. Optional. String specifying a project name.
#' @param maxWait integer. The maximum time to wait for each of two steps: (1) The initial
#'   project creation request, and (2) data processing that occurs after receiving the response
#'   to this initial request.
#' @inherit as.dataRobotProjectShort return
#' @examples
#' \dontrun{
#'   SetupProjectFromHDFS(url = 'hdfs://path/to/data',
#'                        port = 12345,
#'                        projectName = 'dataProject')
#' }
#' @export
SetupProjectFromHDFS <- function(url, port = NULL, projectName = NULL, maxWait = 60 * 60) {
  Deprecated("SetupProjectFromHDFS (use SetupProjectFromDataSource instead)", "2.15", "2.17")
  routeString <- "hdfsProjects/"
  dataList <- list(projectName = projectName,
                   port = port,
                   url = url
 )
  postResponse <- DataRobotPOST(routeString, body = dataList,
                                returnRawResponse = TRUE, timeout = maxWait)
  message(paste("Project", projectName,
                "creation requested, awaiting creation"))
  project <- ProjectFromJobResponse(postResponse, maxWait = maxWait)
  message(sprintf("Project %s (%s) created", project$projectId, project$projectName))
  as.dataRobotProjectShort(project)
}


#' Create a project from a data source.
#'
#' @param dataSourceId character. The ID of the data source to create a project from.
#' @param username character. The username to use for authentication to the database.
#' @param password character. The password to use for authentication to the database.
#' @param projectName character. Optional. String specifying a project name.
#'   The password is encrypted at server side and never saved or stored.
#' @param maxWait integer. The maximum time to wait for each of two steps: (1) The initial
#'   project creation request, and (2) data processing that occurs after receiving the response
#'   to this initial request.
#' @inherit as.dataRobotProjectShort return
#' @examples
#' \dontrun{
#'  dataSourceId <- "5c1303269300d900016b41a7"
#'  SetupProjectFromDataSource(dataSourceId, username = "username", password = "hunter1",
#'                             projectName = "My Project")
#' }
#' @export
SetupProjectFromDataSource <- function(dataSourceId, username, password, projectName = NULL,
                                       maxWait = 60 * 60) {
  if (is(dataSourceId, "dataRobotDataSource")) { dataSourceId <- dataSourceId$id }
  routeString <- "projects/"
  body <- list(dataSourceId = dataSourceId, user = username, password = password)
  if (!is.null(projectName)) { body$projectName <- projectName }
  postResponse <- DataRobotPOST(routeString, body = body,
                                returnRawResponse = TRUE, timeout = maxWait)
  message(paste("Project", projectName,
                "creation requested, awaiting creation"))
  project <- ProjectFromJobResponse(postResponse, maxWait = maxWait)
  message(sprintf("Project %s (%s) created", project$projectId, project$projectName))
  as.dataRobotProjectShort(project)
}


#' Retrieve a project from the job response, which has a project-creation URL
#'
#' If project creation times out, the error message includes a URL corresponding to the project
#' creation task. That URL can be passed to this function (which will return the completed project
#' details when finished) to resume waiting for project creation.
#'
#' @param jobResponse An HTTP POST response that includes a redirect to the
#'   temporary status URL.
#' @param maxWait The maximum time to wait (in seconds) for project creation before aborting.
#' @export
ProjectFromJobResponse <- function(jobResponse, maxWait = 600) {
  timeoutMessage <-
    paste(sprintf("Project creation did not complete before timeout (%ss).", maxWait),
          "To query its status and (if complete) retrieve the completed project, use:\n  ",
          sprintf("%s('%s')", "ProjectFromJobResponse", jobResponse))
  projectInfo <- tryCatch(WaitForAsyncReturn(GetRedirectFromResponse(jobResponse),
                                             addUrl = FALSE,
                                             maxWait = maxWait,
                                             failureStatuses = "ERROR"),
                            AsyncTimeout = function(e) stop(timeoutMessage))
  list(projectName = projectInfo$projectName,
       projectId = projectInfo$id, # NB: This is `id` not `projectId` because it doesn't
                                   # become `projectId` until after `as.dataRobotProject`
       fileName = projectInfo$fileName,
       created = projectInfo$created)
}


encryptedString <- function(plainText, maxWait = 60 * 10) {
  routeString <- "stringEncryptions/"
  dataList <- list(plainText = plainText)
  ret <- DataRobotPOST(routeString, body = dataList, timeout = maxWait)
  ret$cipherText
}

isURL <- function(dataSource) {
  is.character(dataSource) && (substr(dataSource, 1, 4) == "http" ||
                               substr(dataSource, 1, 5) == "file:")
}

#' Return value for SetupProject() and others
#'
#' @param inList list. See return value below for expected elements.
#' @return A named list that contains:
#' \describe{
#'   \item{projectName}{character. The name assigned to the DataRobot project}
#'   \item{projectId}{character. The unique alphanumeric project identifier for this DataRobot
#'     project}
#'   \item{fileName}{character. The name of the CSV modeling file uploaded for this project}
#'   \item{created}{character. The time and date of project creation}
#' }
as.dataRobotProjectShort <- function(inList) {
  elements <- c("projectName", "projectId", "fileName", "created")
  outList <- ApplySchema(inList, elements)
  class(outList) <- "dataRobotProject"
  outList
}
