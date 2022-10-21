#' Returns a dataframe with information on available data stores.
#'
#' @return data.frame containing information on possible data stores.
#' @examples
#' \dontrun{
#'  ListDataStores()
#' }
#' @export
ListDataStores <- function() {
  routeString <- UrlJoin("externalDataStores")
  dataStores <- DataRobotGET(routeString)
  dataStores <- GetServerDataInRows(dataStores)
  as.dataRobotDataStores(dataStores)
}

as.dataRobotDataStores <- function(elements) {
  ApplySchema(elements, c("id", "canonicalName", "type", "updated", "creator", "params"))
}


#' Returns information about a particular data store.
#'
#' @param dataStoreId character. The id of the data store.
#' @return A list containing information on the particular data store:
#' \itemize{
#'   \item id character. The dataStoreId of the data store.
#'   \item canonicalName character. The user-friendly name of the data store.
#'   \item type character. The type of data store.
#'   \item updated datetime. A timestamp for the last time the data store was updated.
#'   \item creator character. The userId of the user who created the data store.
#'   \item params list. A list specifying the data store parameters.
#' }
#' @examples
#' \dontrun{
#'  dataStoreId <- "5c1303269300d900016b41a7"
#'  GetDataStore(dataStoreId)
#' }
#' @export
GetDataStore <- function(dataStoreId) {
  routeString <- UrlJoin("externalDataStores", dataStoreId)
  driver <- DataRobotGET(routeString)
  as.dataRobotDataStore(driver)
}

as.dataRobotDataStore <- function(inList) {
  elements <- c("id", "canonicalName", "type", "updated", "creator", "params")
  outList <- ApplySchema(inList, elements)
  outList$params <- ApplySchema(outList$params, c("driverId", "jdbcUrl"))
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
#'  CreateDataStore(type = "jdbc",
#'                  canonicalName = "Demo DB",
#'                  driverId = "57a7c978c808916f4a630f89",
#'                  jdbcUrl = "jdbc:postgresql://my.db.address.org:5432/my_db")
#' }
#' @export
CreateDataStore <- function(type, canonicalName, driverId, jdbcUrl) {
  body <- list(type = type,
               canonicalName = canonicalName,
               params = list(driverId = driverId,
                             jdbcUrl = jdbcUrl))
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
#'  dataStoreId <- "5c1303269300d900016b41a7"
#'  UpdateDataStore(dataStoreId, canonicalName = "Different Name")
#' }
#' @export
UpdateDataStore <- function(dataStoreId, canonicalName = NULL, driverId = NULL, jdbcUrl = NULL) {
  if (is(dataStoreId, "dataRobotDataStore")) { dataStoreId <- dataStoreId$id }
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
#'  dataStoreId <- "5c1303269300d900016b41a7"
#'  DeleteDataStore(dataStoreId)
#' }
#' @export
DeleteDataStore <- function(dataStoreId) {
  if (is(dataStoreId, "dataRobotDataStore")) { dataStoreId <- dataStoreId$id }
  routeString <- UrlJoin("externalDataStores", dataStoreId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}


#' Test the database connection to the data store.
#'
#' @param dataStoreId character. The ID of the data store to update.
#' @param username character. The username to use for authentication to the database.
#' @param password character. The password to use for authentication to the database.
#'   The password is encrypted at server side and never saved or stored.
#' @examples
#' \dontrun{
#'  dataStoreId <- "5c1303269300d900016b41a7"
#'  TestDataStore(dataStoreId, username = "myUser", password = "mySecurePass129")
#' }
#' @return TRUE if successful, otherwise it will error.
#' @export
TestDataStore <- function(dataStoreId, username, password) {
  if (is(dataStoreId, "dataRobotDataStore")) { dataStoreId <- dataStoreId$id }
  body <- list(user = username,
               password = password)
  routeString <- UrlJoin("externalDataStores", dataStoreId, "test")
  response <- DataRobotPOST(routeString, body = body)
  if (identical(response$message, "Connection successful")) {
    TRUE
  } else {
    stop("Received ", response$message, " from server.")
  }
}


#' Get the schemas associated with a data store.
#'
#' @inheritParams TestDataStore
#' @examples
#' \dontrun{
#'  dataStoreId <- "5c1303269300d900016b41a7"
#'  GetDataStoreSchemas(dataStoreId, username = "myUser", password = "mySecurePass129")
#' }
#' @return A list with the name of the catalog and the name of the schemas.
#' @export
GetDataStoreSchemas <- function(dataStoreId, username, password) {
  if (is(dataStoreId, "dataRobotDataStore")) { dataStoreId <- dataStoreId$id }
  routeString <- UrlJoin("externalDataStores", dataStoreId, "schemas")
  body <- list(user = username, password = password)
  schema <- DataRobotPOST(routeString, body = body)
  as.dataRobotDataStoreSchema(schema)
}
as.dataRobotDataStoreSchema <- function(inList) {
  ApplySchema(inList, c("catalog", "schemas"))
}


#' Get all tables associated with a data store.
#'
#' @inheritParams TestDataStore
#' @param schema character. The name of the schema to reference. Optional.
#' @examples
#' \dontrun{
#'  dataStoreId <- "5c1303269300d900016b41a7"
#'  GetDataStoreTables(dataStoreId, username = "myUser", password = "mySecurePass129")
#' }
#' @return A list with the name of the catalog and the name of the tables.
#' @export
GetDataStoreTables <- function(dataStoreId, username, password, schema = NULL) {
  if (is(dataStoreId, "dataRobotDataStore")) { dataStoreId <- dataStoreId$id }
  routeString <- UrlJoin("externalDataStores", dataStoreId, "tables")
  body <- list(user = username, password = password)
  if (!is.null(schema)) {
    body$schema <- schema
  }
  tables <- DataRobotPOST(routeString, body = body)
  as.dataRobotDataStoreTables(tables)
}

as.dataRobotDataStoreTables <- function(inList) {
  ApplySchema(inList, c("catalog", "tables"))
}
