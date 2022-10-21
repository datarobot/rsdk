#' Pause the DataRobot modeling queue
#'
#' This function pauses the DataRobot modeling queue for a specified project
#'
#' @inheritParams DeleteProject
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   PauseQueue(projectId)
#' }
#' @export
PauseQueue <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "autopilot")
  bodyList <- list(command = "stop")
  response <- DataRobotPOST(routeString, body = bodyList)
  message(paste("Queue for project", projectId, "paused"))
}
