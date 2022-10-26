# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

as.dataRobotDataSources <- function(elements) {
  elements$dataStoreId <- elements$params$dataStoreId
  elements$table <- elements$params$table
  return(elements)
}


as.dataRobotDataSource <- function(inList) {
  outList <- inList
  class(outList) <- "dataRobotDataSource"
  outList
}


#' @name CreateDataSource
#' @details Create a data source.
#'
#' @param type character. The type of data source.
#' @param canonicalName character. The user-friendly name of the data source.
#' @param dataStoreId character. The ID of the data store to connect to.
#' @param query character. A query to execute on the data store to get the data. Optional.
#' @param table character. The specified database table. Optional.
#' @param schema character. The specified database schema. Optional.
#' @param partitionColumn character. The name of the partition column. Optional.
#' @param fetchSize integer. a user specified fetch size in the range [1, 20000]. Optional.
#'   By default a fetchSize will be assigned to balance throughput and memory usage
#' @examples
#' \dontrun{
#' dataStoreId <- "5c1303269300d900016b41a7"
#' CreateDataSource(
#'   type = "jdbc",
#'   canonicalName = "Airline stats after 1995",
#'   dataStoreId = dataStoreId,
#'   query = 'SELECT * FROM airlines10mb WHERE "Year" >= 1995;'
#' )
#' }
#' @export
#' @include data_connectivity_apiWrapper.R
CreateDataSource <- function(type, canonicalName, dataStoreId, query = NULL,
                             table = NULL, schema = NULL, partitionColumn = NULL,
                             fetchSize = NULL) {
  body <- list(
    type = type,
    canonicalName = canonicalName,
    params = list(
      dataStoreId = dataStoreId,
      query = query,
      table = table,
      schema = schema,
      partitionColumn = partitionColumn,
      fetchSize = fetchSize
    )
  )
  body$params <- Filter(Negate(is.null), body$params)
  routeString <- UrlJoin("externalDataSources")
  response <- DataRobotPOST(routeString, body = body, encode = "json")
  as.dataRobotDataSource(response)
}


#' @name UpdateDataSource
#' @details Update a data store.
#'
#' @param dataSourceId character. The ID of the data store to update.
#' @inheritParams CreateDataSource
#' @examples
#' \dontrun{
#' dataSourceId <- "5c1303269300d900016b41a7"
#' UpdateDataSource(dataSourceId, canonicalName = "Different Name")
#' }
#' @export
#' @include data_connectivity_apiWrapper.R
UpdateDataSource <- function(dataSourceId, canonicalName = NULL, dataStoreId = NULL,
                             query = NULL, table = NULL, schema = NULL,
                             partitionColumn = NULL, fetchSize = NULL) {
  if (is(dataSourceId, "dataRobotDataSource")) {
    dataSourceId <- dataSourceId$id
  }
  body <- list(
    canonicalName = canonicalName,
    params = list(
      dataStoreId = dataStoreId,
      query = query,
      table = table,
      schema = schema,
      partitionColumn = partitionColumn,
      fetchSize = fetchSize
    )
  )
  body <- Filter(Negate(is.null), body)
  body$params <- Filter(Negate(is.null), body$params)
  routeString <- UrlJoin("externalDataSources", dataSourceId)
  response <- DataRobotPATCH(routeString, body = body, encode = "json")
  as.dataRobotDataSource(response)
}

#' @name ListDataSources
#' @details Returns a dataframe with information on available data sources.
#'
#' @return data.frame containing information on possible data sources.
#' @examples
#' \dontrun{
#' ListDataSources()
#' }
#' @export
#' @include data_connectivity_apiWrapper.R
ListDataSources

#' @name GetDataSource
#' @details Returns information about a particular data source.
#'
#' @param dataSourceId character. The id of the data source
#' @return A list containing information on the particular data source:
#' \itemize{
#'   \item className character. The Java class name of the driver.
#'   \item baseNames character. A vector of the file name(s) of the jar files.
#'   \item canonicalName character. The user-friendly name of the driver.
#'   \item id character. The dataSourceId of the driver.
#'   \item creator character. The userId of the user who created the driver.
#' }
#' @examples
#' \dontrun{
#' dataSourceId <- "57a7c978c808916f4a630f89"
#' GetDataSource(dataSourceId)
#' }
#' @export
#' @include data_connectivity_apiWrapper.R
GetDataSource

#' @name DeleteDataSource
#' @details Delete a data store.
#'
#' @param dataSourceId character. The ID of the data store to update.
#' @examples
#' \dontrun{
#' dataSourceId <- "5c1303269300d900016b41a7"
#' DeleteDataSource(dataSourceId)
#' }
#' @export
#' @include data_connectivity_apiWrapper.R
DeleteDataSource
