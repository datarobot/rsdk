#' Plot method for DataRobot S3 objects of class listOfModels
#'
#' Method for R's generic plot function for DataRobot S3 objects
#' of class listOfModels.  This function generates a horizontal
#' barplot as described under Details.
#'
#' This function generates a horizontal barplot with one bar for
#' each model characterized in the 'listOfModels' object x.
#' The length of each bar is specified by the value of metric;
#' if this parameter is specified as NULL (the default), the
#' project fitting metric is used, as determined by the projectMetric
#' value from the first element of x.  Text is added to each bar in
#' the plot, centered at the position specified by the xpos parameter,
#' based on the value of the modelType element of each model in the
#' list x.  The size and color of these text labels may be controlled
#' with the textSize and textColor parameters.  The order in which
#' these models appear on the plot is controlled by the choice of
#' metric and the value of the orderDecreasing parameter, and subsets
#' of the models appearing in the list x may be selected via the pct
#' and selectRecords parameters.
#'
#' @param x S3 object of class listOfModels to be plotted.
#' @param y Not used; included for conformance with plot() generic
#' function parameter requirements.
#' @param metric character. Optional. Defines the metric to be used in
#'   constructing the barplot.  If NULL (the default), the validation
#'   set value for the project fitting metric is used; otherwise, this
#'   value must name one of the elements of the metrics list associated
#'   with each model in x.
#' @param pct integer. Optional. Specifies a samplePct value used in selecting
#'   models to include in the barplot summary.  If NULL (the default),
#'   all project models are included.  Note, however, that this list of
#'   models is intersected with the list of models defined by the
#'   selectRecords parameter, so that only those models identified by
#'   both selectRecords and pct appear in the plot.
#' @param selectRecords integer. Optional. A vector that specifies the individual
#'   elements of the list x to be included in the barplot summary.
#'   If NULL (the default), all models are included.  Note, however,
#'   that this list of models is intersected with the list of models
#'   defined by the pct parameter, so that only those models identified
#'   by both selectRecords and pct appear in the plot.
#' @param orderDecreasing logical. Optional. If TRUE, the barplot is built from
#'   the bottom up in decreasing order of the metric values; if FALSE,
#'   the barplot is built in increasing order of metric values.
#'   The default is NULL, which causes the plot to be generated in the
#'   order in which the models appear in the list x.
#' @param textSize numeric. Optional. Multiplicative scaling factor for the
#'   model name labels on the barplot.
#' @param textColor character. Optional. If character, this parameter specifies the
#'   text color used in labelling all models in the barplot; if a character
#'   vector, it specifies one color for each model in the plot.
#' @param borderColor character. Optional. Specifies the border color for all
#'   bars in the barplot, surrounding a transparent background.
#' @param xpos numeric. Optional. Defines the horizontal position of the center of
#'   all text labels on the plot.
#'   The default is NULL, which causes all text to be centered in
#'   the plot; if xpos is a single number, all text labels are centered
#'   at this position; if xpos is a vector, it specifies one center
#'   position for each model in the plot.
#' @param \dots list. Optional. Additional named parameters to be passed to R's barplot
#'   function used in generating the plot
#' @return None. This function is called for its side-effect of
#'   generating a plot.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   plot(ListModels(projectId))
#' }
#' @export
plot.listOfModels <- function(x, y, metric = NULL, pct = NULL,
                              selectRecords = NULL, orderDecreasing = NULL,
                              textSize = 0.8, textColor = "black",
                              borderColor = "blue", xpos = NULL, ...) {
  oFrame <- as.data.frame(x, simple = FALSE)

  #  If metric = NULL (the default), use projectMetric for validation data
  #     from the first element of object
  if (is.null(metric)) {
    metricName <- paste(oFrame$projectMetric[1], "validation", sep = ".")
  } else {
    metricName <- metric
  }
  metricIndex <- which(colnames(oFrame) == metricName)

  #  If pct and/or selectRecords are not null,
  #       use them to generate a row selection index
  if (!is.null(pct)) {
    pctIndex <- which(oFrame$samplePct == pct)
  } else {
    pctIndex <- seq(1, nrow(oFrame), 1)
  }
  if (length(pctIndex) == 0) { stop("The requested `pct` was not found among the models.") }
  if (!is.null(selectRecords)) {
    keepIndex <- intersect(selectRecords, pctIndex)
  } else {
    keepIndex <- pctIndex
  }
  plotMetric <- oFrame[keepIndex, metricIndex]
  modelTypes <- oFrame$modelType[keepIndex]

  #  If orderDecreasing is not NULL, it is a logical value
  #  for the sort 'decreasing' parameter; otherwise,
  #  no reordering is done
  if (!is.null(orderDecreasing)) {
    plotIndex <- order(plotMetric, decreasing = orderDecreasing)
  } else {
    plotIndex <- seq(1, length(plotMetric), 1)
  }
  plotMetric <- plotMetric[plotIndex]
  modelTypes <- modelTypes[plotIndex]

  #  Generate a horizontal barplot
  if (is.null(xpos)) {
    xpos <- max(plotMetric, na.rm = TRUE) / 2
  }
  mids <- barplot(plotMetric, horiz = TRUE, col = "transparent",
                  border = borderColor, xlab = metricName, ...)
  text(xpos, mids, modelTypes, cex = textSize, col = textColor)
}
