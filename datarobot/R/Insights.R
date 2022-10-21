#' An internal function to help fetch insights.
#'
#' See \code{GetLiftChart}, \code{GetRocCurve}, \code{GetResidualsChart} for details.
#' @inheritParams GetLiftChart
#' @param method character. The API URL to use to get insight information.
GetGeneralizedInsight <- function(method, model,
                                  source = DataPartition$VALIDATION,
                                  fallbackToParentInsights = FALSE) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  if (!is.null(source)) {
    routeString <- UrlJoin("projects", projectId, "models", modelId, method, source)
  } else {
    routeString <- UrlJoin("projects", projectId, "models", modelId, method)
  }
  tryCatch(DataRobotGET(routeString),
           error = function(e) {
                     if (grepl("404", as.character(e)) && isTRUE(fallbackToParentInsights)) {
                       currentModel <- GetFrozenModel(projectId, modelId)
                       parentModel <- GetModel(currentModel$projectId, currentModel$parentModelId)
                       GetGeneralizedInsight(method, parentModel, source = source,
                                             fallbackToParentInsights = FALSE)
                     } else {
                       stop(e)
                     }
                   })
}
