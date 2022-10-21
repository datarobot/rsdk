#' Retrieve a model's confusion chart for a specified source.
#'
#' @inheritParams GetLiftChart
#' @return data.frame with the following components:
#' \itemize{
#'   \item source character. The name of the source of the confusion chart. Will be a member of
#'     \code{DataPartition}.
#'   \item data list. The data for the confusion chart, containing:
#'     \itemize{
#'       \item classes character. A vector containing the names of all the classes.
#'       \item confusionMatrix matrix. A matrix showing the actual versus the predicted class
#'         values.
#'       \item classMetrics list. A list detailing further metrics for each class:
#'         \itemize{
#'           \item wasActualPercentages data.frame. A dataframe detailing the actual percentage
#'             distribution of the classes.
#'           \item wasPredictedPercentages data.frame. A dataframe detailing the predicted
#'             distribution of the classes.
#'           \item f1 numeric. The F1 score for the predictions of the class.
#'           \item recall numeric. The recall score for the predictions of the class.
#'           \item precision numeric. The precision score for the predictions of the class.
#'           \item actualCount integer. The actual count of values for the class.
#'           \item predictedCount integer. The predicted count of values for the class.
#'           \item className character. A vector containing the name of the class.
#'         }
#'     }
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetModel(projectId, modelId)
#'   GetConfusionChart(modelId, source = DataPartition$VALIDATION)
#' }
#' @export
GetConfusionChart <- function(model, source = DataPartition$VALIDATION,
                              fallbackToParentInsights = FALSE) {
  response <- GetGeneralizedInsight("confusionCharts",
                                    model,
                                    source = source,
                                    fallbackToParentInsights = fallbackToParentInsights)
  as.dataRobotConfusionChart(response)
}

#' Returns all available confusion charts for the model.
#'
#' Note that the confusion chart for \code{source = "crossValidation"} will not be available
#' unless cross validation has been run for that model. Also, the confusion chart
#' for \code{source = "holdout"} will not be available unless the holdout has been unlocked for
#' the project.
#'
#' @inheritParams GetConfusionChart
#' @return A list of all confusion charts for the model, one for each partition type
#'   found in \code{DataPartition}.
#' @examples
#' \dontrun{
#'   modelId <- "5996f820af07fc605e81ead4"
#'   ListConfusionCharts(modelId)
#' }
#' @export
ListConfusionCharts <- function(model, fallbackToParentInsights = FALSE) {
  response <- GetGeneralizedInsight("confusionCharts",
                                    model,
                                    source = NULL,
                                    fallbackToParentInsights = fallbackToParentInsights)
  # Charts come back as a dataframe with each column containing a list for that column,
  # need to reformat it as a list where each entry has all the data for a given chart.
  charts <- list()
  for (i in seq(nrow(response$charts))) {
    chart <- as.dataRobotConfusionChart(response$charts[i, ])
    chart$data$classMetrics <- as.list(chart$data$classMetrics[[1]])
    chart$data$classes <- chart$data$classes[[1]]
    chart$data$confusionMatrix <- chart$data$confusionMatrix[[1]]
    charts <- append(charts, list(chart))
  }
  charts
}

as.dataRobotConfusionChart <- function(inList) {
  outList <- as.list(inList)
  outList <- ApplySchema(outList, c("data", "source"))
  outList$data <- as.list(outList$data)
  outList$data <- ApplySchema(outList$data, c("classes", "classMetrics", "confusionMatrix"))
  outList$data$classMetrics <- as.list(outList$data$classMetrics)
  outList
}
