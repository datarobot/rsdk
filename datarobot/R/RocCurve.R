#' Retrieve ROC curve data for a model for a particular data partition (see DataPartition)
#'
#' @inheritParams GetLiftChart
#' @return list with the following components:
#' \itemize{
#'   \item source. Character: data partition for which ROC curve data is returned
#'     (see DataPartition).
#'   \item negativeClassPredictions. Numeric: example predictions for the negative class.
#'   \item rocPoints. data.frame: each row represents pre-calculated metrics (accuracy,
#'     f1_score, false_negative_score, true_negative_score, true_positive_score,
#'     false_positive_score, true_negative_rate, false_positive_rate, true_positive_rate,
#'     matthews_correlation_coefficient, positive_predictive_value, negative_predictive_value,
#'     threshold) associated with different thresholds for the ROC curve.
#'   \item positiveClassPredictions. Numeric: example predictions for the positive class.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   GetRocCurve(model)
#' }
#' @export
GetRocCurve <- function(model, source = DataPartition$VALIDATION,
                        fallbackToParentInsights = FALSE) {
  as.dataRobotRocCurve(GetGeneralizedInsight("rocCurve",
                                             model,
                                             source = source,
                                             fallbackToParentInsights = fallbackToParentInsights))
}

as.dataRobotRocCurve <- function(inList) {
  rocElements <- c("source",
                   "negativeClassPredictions",
                   "rocPoints",
                   "positiveClassPredictions")
  rocList <- ApplySchema(inList, rocElements)
  rocPointsElements <- c("accuracy", "f1Score", "falseNegativeScore",
                         "trueNegativeScore", "truePositiveScore",
                         "falsePositiveScore", "trueNegativeRate",
                         "falsePositiveRate", "truePositiveRate",
                         "matthewsCorrelationCoefficient", "positivePredictiveValue",
                         "negativePredictiveValue", "threshold",
                         "fractionPredictedAsPositive", "fractionPredictedAsNegative",
                         "liftPositive", "liftNegative")
  rocList$rocPoints <- ApplySchema(rocList$rocPoints, rocPointsElements)
  rocList
}


#' Retrieve ROC curve data for a model for all available data partitions (see DataPartition)
#'
#' @inheritParams GetLiftChart
#' @return list of lists where each list is renamed as the data partitions source and returns the
#'   following components:
#' \itemize{
#'   \item source. Character: data partitions for which ROC curve data is returned
#'     (see DataPartition).
#'   \item negativeClassPredictions. Numeric: example predictions for the negative class for each
#'     data partition source.
#'   \item rocPoints. data.frame: each row represents pre-calculated metrics (accuracy, f1_score,
#'     false_negative_score, true_negative_score, true_positive_score, false_positive_score,
#'   true_negative_rate, false_positive_rate, true_positive_rate, matthews_correlation_coefficient,
#'     positive_predictive_value, negative_predictive_value, threshold) associated with different
#'     thresholds for the ROC curve.
#'   \item positiveClassPredictions. Numeric: example predictions for the positive class for each
#'     data partition source.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   ListRocCurves(model)
#' }
#' @export
ListRocCurves <- function(model, fallbackToParentInsights = FALSE) {
  response <- GetGeneralizedInsight("rocCurve",
                                    model,
                                    source = NULL,
                                    fallbackToParentInsights = fallbackToParentInsights)
  temp <- list()
  for (i in 1:nrow(response$charts)) {
    temp[[i]] <- list(source = response$charts$source[i],
                      negativeClassPredictions = response$charts$negativeClassPredictions[[i]],
                      rocPoints = response$charts$rocPoints[[i]],
                      positiveClassPredictions = response$charts$positiveClassPredictions[[i]])
  }
  names(temp) <- response$charts$source
  response$charts <- temp
  lapply(response$charts, as.dataRobotRocCurve)
}
