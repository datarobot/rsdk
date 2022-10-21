#' Re-start the DataRobot modeling queue
#'
#' This function unpauses the modeling queue for a specified
#' DataRobot project.
#'
#' @inheritParams DeleteProject
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   UnpauseQueue(projectId)
#' }
#' @export
UnpauseQueue <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "autopilot")
  body <- jsonlite::unbox(data.frame(command = "start"))
  response <- DataRobotPOST(routeString, body = body, encode = "json")
  message(paste("Queue for project", projectId, "unpaused"))
}
