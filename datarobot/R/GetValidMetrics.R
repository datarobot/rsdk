#' Retrieve the valid fitting metrics for a specified project and target
#'
#' For the response variable defined by the character string target
#' and the project defined by the parameter project, return the vector
#' of metric names that can be specified for fitting models in this project.
#' This function is intended for use after SetupProject has been run but
#' before SetTarget, allowing the user to specify valid non-default
#' values for the metric parameter.
#'
#' @inheritParams DeleteProject
#' @param target character. String giving the name of the response variable
#' to be predicted by all project models.
#' @return Character vector containing the names of the metric values
#' that are valid for a subsequent call to the SetTarget function.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetValidMetrics(projectId, "targetFeature")
#' }
#' @export
GetValidMetrics <- function(project, target) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "features", "metrics")
  response <- DataRobotGET(routeString, query = list(featureName = target))
  response$availableMetrics
}
