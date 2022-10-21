#' Retrieve residuals chart data for a model for a data partition (see DataPartition).
#'
#' @param model dataRobotModel. A DataRobot model object like that returned by `GetModel`. The model
#'   must be a regression model that is not time-aware.
#' @param source character. The data partition for which data would be returned. Default is
#'   `DataPartition$VALIDATION`. See `DataPartition` for details.
#' @param fallbackToParentInsights logical. If TRUE, this will return the residuals chart data for
#'   the model's parent if the residuals chart is not available for the model and the model has a
#'   parent model.
#' @return list with a single object containing residuals chart data whose name matches the source
#' requested. See `DataPartition` for details. This object has the following components:
#' \itemize{
#'   \item residualMean. Numeric: the arithmetic mean of the predicted value minus the actual value
#'     over the downsampled dataset.
#'   \item coefficientOfDetermination. Numeric: aka the r-squared value. This value is calculated
#'     over the downsampled output, not the full input.
#'   \item data. data.frame:  The rows of chart data in `[actual, predicted, residual, rowNumber]`
#'     form. If the row number was not available at the time of model creation, or if working with
#'     DataRobot 5.2, which does not provide rowNumber in the API response, the rowNumber will be
#'     NA.
#'   \item histogram. list: Data to plot a histogram of residual values. Each object contains:
#'     \itemize{
#'       \item intervalStart. Numeric: Start value for an interval, inclusive.
#'       \item intervalEnd. Numeric: End value for an interval, exclusive for all but the last
#'         interval.
#'       \item occurrences. Integer: the number of times the predicted value fits within the
#'         interval.
#'     }
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   GetResidualsChart(model, source = DataPartition$VALIDATION)
#' }
#' @md
#' @export
GetResidualsChart <- function(model, source = DataPartition$VALIDATION,
                         fallbackToParentInsights = FALSE) {
  response <- GetGeneralizedInsight("residuals", model, source = source,
                                    fallbackToParentInsights = fallbackToParentInsights)
  lapply(response$residuals, as.dataRobotResidualsChart)
}

as.dataRobotResidualsChart <- function(inList) {
  outList <- inList
  outList$data <- as.data.frame(outList$data)
  if (ncol(outList$data) == 3) {
    # DR 5.2 does not provide rowNumber, only actual+predicted+residual, so
    # let's check and add it as NA if needed
    outList$data["rowNumber"] <- NA
  }
  colnames(outList$data) <- c("actual",
                             "predicted",
                             "residual",
                             "rowNumber")
  outList$data$rowNumber <- as.integer(outList$data$rowNumber)
  outList$histogram$occurrences <- as.integer(outList$histogram$occurrences)
  class(outList) <- "dataRobotResiduals"
  outList
}

#' Retrieve residuals chart data for a model for all available data partitions (see
#' \code{DataPartition}). This chart is only available for regression models that are not
#' time-aware.
#'
#' @inheritParams GetResidualsChart
#' @return list of objects containing residuals chart data for all available data partitions. See
#' \code{DataPartition} for details. Each object has the following components:
#' \itemize{
#'   \item residualMean. Numeric: the arithmetic mean of the predicted value minus the actual value
#'     over the downsampled dataset.
#'   \item coefficientOfDetermination. Numeric: aka the r-squared value. This value is calculated
#'     over the downsampled output, not the full input.
#'   \item data. data.frame:  The rows of chart data in [actual, predicted, residual, row number]
#'     form. If the row number was not available at the time of model creation, the row number will
#'     be null.
#'   \item histogram. list: Data to plot a histogram of residual values. Each object contains:
#'     \itemize{
#'       \item intervalStart. Numeric: Start value for an interval, inclusive.
#'       \item intervalEnd. Numeric: End value for an interval, exclusive for all but the last
#'         interval.
#'       \item occurrences. Integer: the number of times the predicted value fits within the
#'         interval.
#'     }
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   ListResidualsCharts(model)
#' }
#' @export
ListResidualsCharts <- function(model, fallbackToParentInsights = FALSE) {
  response <- GetGeneralizedInsight("residuals", model, source = NULL,
                                    fallbackToParentInsights = fallbackToParentInsights)
  lapply(response$residuals, as.dataRobotResidualsChart)
}
