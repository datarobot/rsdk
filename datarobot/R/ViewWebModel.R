MakeUrl <- function(parsedUrl, project = NULL, model = NULL) {
  parsedUrl <- paste0(parsedUrl$scheme, "://", parsedUrl$hostname, "/")
  if (is.null(project) && is.null(model)) { stop("Must pass either project or model.") }
  projectId <- if (is.null(project)) { model$projectId } else { project$projectId }
  routeString <- UrlJoin(parsedUrl, "projects", projectId)
  if (!is.null(model)) {
    routeString <- UrlJoin(routeString, "models", model$modelId, "blueprint")
  } else {
    routeString <- UrlJoin(routeString, "eda")
  }
  routeString
}

DataRobotBrowse <- function(routeString) {
  browseURL(routeString)
}

#' Retrieve a DataRobot web page that displays detailed model information
#'
#' This function brings up a web page that displays detailed model
#' information like that available from the standard DataRobot user
#' interface (e.g., graphical representations of model structures).
#'
#' @inheritParams DeleteModel
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   ViewWebModel(model)
#' }
#' @export
ViewWebModel <- function(model) {
  validModel <- ValidateModel(model)
  dataRobotUrl <- Sys.getenv("DATAROBOT_API_ENDPOINT")
  parsedUrl <- httr::parse_url(dataRobotUrl)
  urlString <- MakeUrl(parsedUrl, model = model)
  DataRobotBrowse(urlString)
  modelType <- validModel$modelType
  if (is.null(modelType)) {
    message(paste("Opened URL", urlString, "for selected model"))
  } else {
    message(paste("Opened URL", urlString, "for model:", modelType))
  }
}
