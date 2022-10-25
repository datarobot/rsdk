# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' Retrieve time series properties for a potential multiseries datetime partition column
#'
#' Multiseries time series projects use multiseries id columns to model multiple distinct
#' series within a single project. This function returns the time series properties
#' (time step and time unit) of this column if it were used as a datetime partition column
#' with the specified multiseries id columns, running multiseries detection automatically if
#' it had not previously been successfully ran.
#'
#' @inheritParams DeleteProject
#' @inheritParams RequestCrossSeriesDetection
#' @param maxWait integer. if a multiseries detection task is run, the maximum amount of time to
#' wait for it to complete before giving up.
#' @inherit as.dataRobotMultiSeriesProperties return
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' GetMultiSeriesProperties(projectId,
#'   dateColumn = "myFeature",
#'   multiseriesIdColumns = "Store"
#' )
#' }
#'
#' @family MultiSeriesProject functions
#' @export
GetMultiSeriesProperties <- function(project, dateColumn, multiseriesIdColumns,
                                     crossSeriesGroupByColumns = NULL, maxWait = 600) {
  projectId <- ValidateProject(project)
  featureForUrl <- if (is.character(dateColumn)) {
    URLencode(enc2utf8(dateColumn))
  } else {
    dateColumn
  }

  if (length(multiseriesIdColumns) > 1) {
    stop("Currently only one multiseries id column is supported.")
  }
  if (!is.list(multiseriesIdColumns)) {
    multiseriesIdColumns <- list(multiseriesIdColumns)
  }

  if (length(crossSeriesGroupByColumns) > 1) {
    stop("Currently only one cross series group by column is supported.")
  }
  if (!is.list(crossSeriesGroupByColumns) && !is.null(crossSeriesGroupByColumns)) {
    crossSeriesGroupByColumns <- list(crossSeriesGroupByColumns)
  }

  routeString <- UrlJoin("projects", projectId, "features", featureForUrl, "multiseriesProperties")
  multiseriesProperties <- DataRobotGET(routeString, simplifyDataFrame = TRUE)
  multiseriesProperties <- multiseriesProperties$detectedMultiseriesIdColumns

  if (!is.null(multiseriesProperties)) {
    if (identical(multiseriesProperties, list())) {
      timeSeriesEligible <- FALSE
      timeUnit <- NULL
      timeStep <- NULL
    } else {
      detectedSubset <- multiseriesProperties[
        multiseriesProperties$multiseriesIdColumns ==
          multiseriesIdColumns[[1]],
      ]
      if (NROW(detectedSubset) == 0) {
        timeSeriesEligible <- FALSE
        timeUnit <- NULL
        timeStep <- NULL
      } else {
        timeSeriesEligible <- TRUE
        timeUnit <- unlist(detectedSubset$timeUnit)
        timeStep <- unlist(detectedSubset$timeStep)
      }
    }
  }

  if (!is.null(crossSeriesGroupByColumns)) {
    routeString <- UrlJoin(
      "projects", projectId, "multiseriesIds", featureForUrl,
      "crossSeriesProperties"
    )
    query <- list(crossSeriesGroupByColumns = crossSeriesGroupByColumns)
    crossSeriesProperties <- DataRobotGET(routeString, simplifyDataFrame = TRUE, query = query)
    crossSeriesProperties <- crossSeriesProperties$crossSeriesGroupByColumns
    detectedSubset <- crossSeriesProperties[crossSeriesProperties$name ==
      crossSeriesGroupByColumns[[1]], ]
    crossSeriesEligibility <- detectedSubset$eligibility
    crossSeriesEligible <- detectedSubset$isEligible
    if (!isTRUE(crossSeriesEligible)) {
      crossSeriesEligible <- FALSE
    }
  } else {
    crossSeriesEligibility <- NULL
    crossSeriesEligible <- NULL
  }

  as.dataRobotMultiSeriesProperties(list(
    "timeSeriesEligible" = timeSeriesEligible,
    "crossSeriesEligible" = crossSeriesEligible,
    "crossSeriesEligibilityReason" = crossSeriesEligibility,
    "timeUnit" = timeUnit,
    "timeStep" = timeStep
  ))
}

#' Return value for GetMultiSeriesProperties() and others
#'
#' @param inList list. See return value below for expected elements.
#' @return A named list which contains:
#' \itemize{
#'   \item timeSeriesEligible logical. Whether or not the series is eligible to be used for
#'     time series.
#'   \item crossSeriesEligible logical. Whether or not the cross series group by column is
#'     eligible for cross-series modeling. Will be NULL if no cross series group by column
#'     is used.
#'   \item crossSeriesEligibilityReason character. The type of cross series eligibility
#'     (or ineligibility).
#'   \item timeUnit character. For time series eligible features, the time unit covered by a
#'     single time step, e.g. "HOUR", or NULL for features that are not time series eligible.
#'   \item timeStep integer. Expected difference in time units between rows in the data.
#'     Will be NULL for features that are not time series eligible.
#' }
#' @family MultiSeriesProject functions
as.dataRobotMultiSeriesProperties <- function(inList) {
  outList <- ApplySchema(inList, c(
    "timeSeriesEligible",
    "crossSeriesEligible",
    "crossSeriesEligibilityReason",
    "timeUnit",
    "timeStep"
  ))
  outList
}
