# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

as.dataRobotDataStore <- function(inList) {
  outList <- inList
  class(outList) <- "dataRobotDataStore"
  outList
}


#' Create a data store.
#'
#' @param type character. The type of data store.
#' @param canonicalName character. The user-friendly name of the data store.
#' @param driverId character. The ID of the driver to use.
#' @param jdbcUrl character. The full JDBC url.
#' @examples
#' \dontrun{
#' CreateDataStore(
#'   type = "jdbc",
#'   canonicalName = "Demo DB",
#'   driverId = "57a7c978c808916f4a630f89",
#'   jdbcUrl = "jdbc:postgresql://my.db.address.org:5432/my_db"
#' )
#' }
#' @export
CreateDataStore <- function(type, canonicalName, driverId, jdbcUrl) {
  body <- list(
    type = type,
    canonicalName = canonicalName,
    params = list(
      driverId = driverId,
      jdbcUrl = jdbcUrl
    )
  )
  routeString <- UrlJoin("externalDataStores")
  response <- DataRobotPOST(routeString, body = body, encode = "json")
  as.dataRobotDataStore(response)
}


#' Update a data store.
#'
#' @param dataStoreId character. The ID of the data store to update.
#' @inheritParams CreateDataStore
#' @examples
#' \dontrun{
#' dataStoreId <- "5c1303269300d900016b41a7"
#' UpdateDataStore(dataStoreId, canonicalName = "Different Name")
#' }
#' @export
UpdateDataStore <- function(dataStoreId, canonicalName = NULL, driverId = NULL, jdbcUrl = NULL) {
  if (is(dataStoreId, "dataRobotDataStore")) {
    dataStoreId <- dataStoreId$id
  }
  params <- list(driverId = driverId, jdbcUrl = jdbcUrl)
  params <- Filter(Negate(is.null), params)
  body <- list(canonicalName = canonicalName)
  body <- Filter(Negate(is.null), body)
  body$params <- params
  routeString <- UrlJoin("externalDataStores", dataStoreId)
  response <- DataRobotPATCH(routeString, body = body, encode = "json")
  as.dataRobotDataStore(response)
}


#' Delete a data store.
#'
#' @param dataStoreId character. The ID of the data store to update.
#' @examples
#' \dontrun{
#' dataStoreId <- "5c1303269300d900016b41a7"
#' DeleteDataStore(dataStoreId)
#' }
#' @export
DeleteDataStore <- function(dataStoreId) {
  if (is(dataStoreId, "dataRobotDataStore")) {
    dataStoreId <- dataStoreId$id
  }
  routeString <- UrlJoin("externalDataStores", dataStoreId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}
