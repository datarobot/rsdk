#' Get a report on missing values for the model.
#'
#' The missing values report is a list of items, one per feature, sorted by
#' missing count in descending order. Each item in the report contains details on the number
#' of missing values for that feature and how they were handled by the model.
#'
#' @inheritParams GetModel
#' @return A list containing:
#' \itemize{
#'   \item feature character. The name of the feature.
#'   \item type character. Feature type (numeric or categorical).
#'   \item missingCount numeric. The number of missing values in the training data for
#'     that feature.
#'   \item missingPercentage numeric. The percentage of missing values in the training
#'     data for the feature.
#'   \item tasks list. A list of information on each task that was applied to that feature
#'     to handle missing values. This information contains:
#'     \itemize{
#'       \item id character. The id of the node in the model blueprint chart for this task.
#'         (See \link{GetBlueprintChart} for more information on blueprint charts.)
#'       \item name character. The name of the task.
#'       \item descriptions character. Aggregated information about how the task handles
#'         missing values.
#'     }
#' }
#' @examples
#' \dontrun{
#'    projectId <- "5984b4d7100d2b31c1166529"
#'    modelId <- "5984b4d7100d2b31c1166529"
#'    GetMissingValuesReport(projectId, modelId)
#' }
#' @export
GetMissingValuesReport <- function(project, modelId) {
  projectId <- ValidateProject(project)
  if (!is.character(modelId)) { stop("Invalid modelId") }
  routeString <- UrlJoin("projects", projectId, "models", modelId, "missingReport")
  result <- DataRobotGET(routeString, simplify = FALSE)
  as.dataRobotMissingValuesReport(result$missingValuesReport)
}

# Process the missing values report
#
# Input is:
# list(list(missingCount = <missingCount>,
#           tasks = list(<taskId> = list(descriptions = list(<descriptions>),
#                                        name = <name>),
#           type = <type>,
#           feature = <featureName>,
#           missingPercentage = <missingPercentage>), ...)
# Output is:
# list(<featureName> = list(type = <featureType>,
#                           missingCount = <missingCount>,
#                           missingPercentage = <missingPercentage>,
#                           tasks = list(list(name = <Task1Name>,
#                                             descriptions = c(<Task1Description>, ...),
#                                             id = <taskId>), ...)), ...)
as.dataRobotMissingValuesReport <- function(inList) {
  features <- lapply(inList, `[[`, "feature")
  for (i in seq_along(inList)) {
    inList[[i]]$feature <- NULL # Drop feature from within list
    for (j in seq_along(inList[[i]]$tasks)) {
      inList[[i]]$tasks[[j]]$id <- names(inList[[i]]$tasks)[[j]]
      inList[[i]]$tasks[[j]]$name <- as.character(inList[[i]]$tasks[[j]]$name)
      inList[[i]]$tasks[[j]]$descriptions <- as.character(inList[[i]]$tasks[[j]]$descriptions)
    }
    inList[[i]]$tasks <- unname(inList[[i]]$tasks)
  }
  stats::setNames(inList, features)
}
