# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

as.dataRobotDrivers <- function(elements) {
  elements[, c("id", "canonicalName", "className", "baseNames", "creator")]
}


as.dataRobotDriver <- function(inList) {
  outList <- inList
  class(outList) <- "dataRobotDriver"
  outList
}

#' @name ListDrivers
#' @details Returns a dataframe with information on available drivers.
#'
#' @return data.frame containing information on possible drivers.
#' @examples
#' \dontrun{
#' ListDrivers()
#' }
#' @export
#' @include data_connectivity_apiWrapper.R
ListDrivers

#' @name GetDriver
#' @details Returns information about a particular driver.
#'
#' @param driverId character. The id of the driver.
#' @return A list containing information on the particular driver:
#' \itemize{
#'   \item className character. The Java class name of the driver.
#'   \item baseNames character. A vector of the file name(s) of the jar files.
#'   \item canonicalName character. The user-friendly name of the driver.
#'   \item id character. The driverId of the driver.
#'   \item creator character. The userId of the user who created the driver.
#' }
#' @examples
#' \dontrun{
#' driverId <- "57a7c978c808916f4a630f89"
#' GetDriver(driverId)
#' }
#' @export
#' @include data_connectivity_apiWrapper.R
GetDriver
