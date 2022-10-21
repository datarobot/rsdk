#' replace NULL in $metrics list elements with NA
#' @param metricsList list. List of metrics to reformat.
ReformatMetrics <- function(metricsList) {
  #
  ############################################################################
  #
  #  The $metrics element returned by the Public API server in response to a
  #  GetModel request has missing metric values (e.g., holdout or
  #  crossValidation values) coded as NULL, which causes undesirable
  #  side-effects (e.g., assigning a list element the NULL value deletes the
  #  list element). To prevent these side-effects, this function converts NULL
  #  values to NA values, the standard representation for missing values in R.
  #
  ############################################################################

  nList <- length(metricsList)
  outList <- vector("list", nList)

  ReplaceFunction <- function(x) { ifelse(is.null(x), as.numeric(NA), x) }

  for (i in 1:nList) {
    outList[[i]] <- as.data.frame(lapply(metricsList[[i]], ReplaceFunction))
  }

  names(outList) <- names(metricsList)
  return(outList)
}
