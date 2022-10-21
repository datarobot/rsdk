#' Star a model.
#'
#' @inheritParams GetFeatureImpact
#' @return the model object, but now starred
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   StarModel(model)
#' }
#' @export
StarModel <- function(model) {
  model <- ValidateModel(model)
  modelId <- model$modelId
  projectId <- ValidateProject(model$projectId)
  routeString <- UrlJoin("projects", projectId, "models", modelId)
  body <- list("isStarred" = TRUE)
  DataRobotPATCH(routeString, body = body, encode = "json")
  GetModel(projectId, modelId)
}

#' Unstar a model.
#'
#' @inheritParams GetFeatureImpact
#' @return the model object, but now unstarred
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   UnstarModel(model)
#' }
#' @export
UnstarModel <- function(model) {
  model <- ValidateModel(model)
  modelId <- model$modelId
  projectId <- ValidateProject(model$projectId)
  routeString <- UrlJoin("projects", projectId, "models", modelId)
  body <- list("isStarred" = FALSE)
  DataRobotPATCH(routeString, body = body, encode = "json")
  GetModel(projectId, modelId)
}

#' Star a model if it is unstarred, otherwise unstar the model.
#'
#' @inheritParams GetFeatureImpact
#' @return the model object, but now starred if unstarred or unstarred if starred.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   ToggleStarForModel(model)
#' }
#' @export
ToggleStarForModel <- function(model) {
  model <- ValidateModel(model)
  if (isTRUE(model$isStarred)) { UnstarModel(model) } else { StarModel(model) }
}

#' List all the starred models in a project.
#'
#' Star models and add them to this list using \code{StarModel} or \code{ToggleStarForModel}.
#' Unstar models and remove them from this list using \code{UnstarModel} or
#' \code{ToggleStarForModel}
#'
#' @inheritParams ListModels
#' @inherit ListModels return
#' @examples
#' \dontrun{
#'    projectId <- "59a5af20c80891534e3c2bde"
#'    ListStarredModels(projectId)
#' }
#' @export
ListStarredModels <- function(project, orderBy = NULL) {
  ListModels(project, orderBy = orderBy, filter = list("isStarred" = TRUE))
}
