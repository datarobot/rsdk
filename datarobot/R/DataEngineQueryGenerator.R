# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
as.queryGeneratorDataset <- function(inList) {
  outList <- inList
  class(outList) <- "queryGeneratorDataset"
  outList
}

as.queryGeneratorSettings <- function(inList) {
  outList <- inList
  class(outList) <- "queryGeneratorSettings"
  outList
}

as.dataRobotQueryGenerator <- function(inList) {
  outList <- inList
  outList$datasets <- lapply(outList$datasets, as.queryGeneratorDataset)
  outList$generatorSettings <- as.queryGeneratorSettings(outList$generatorSettings)
  class(outList) <- "dataRobotQueryGenerator"
  outList
}

#' Create a DataEngineQueryGenerator for time series data prep.
#'
#' @param generatorType character. The type of query generator to create. "TimeSeries" is supported.
#' @param dataset object of class \code{queryGeneratorDataset}.
#' @param generatorSettings object of class \code{queryGeneratorSettings}.

#' @return A query generator object:
#' \itemize{
#'   \item Id. character. The ID of the created query generator.
#'   \item datasets. list of objects containing elements
#'     \itemize{
#'       \item alias. character. The alias for the dataset used in the query.
#'       \item datasetId. character. The ID of the dataset.
#'       \item datasetVersionId. character. The ID of the dataset version.
#'     }
#'   \item generatorType. character. The type of the query generator.
#'   \item generatorSettings. object containing elements
#'     \itemize{
#'       \item datetimePartitionColumn. character. The name of the date column.
#'       \item timeUnit. character. The unit of the time step.
#'       \item timeStep. integer. The number of units in the time step.
#'       \item defaultNumericAggregationMethod. character. Aggregation for numeric columns.
#'       \item defaultCategoricalAggregationMethod. character. Aggregation for categorical columns.
#'       \item target. character. The name of the target column.
#'       \item multiseriesIdColumns. list of character. Optional. The name of the series id column.
#'       \item defaultTextAggregationMethod. character. Optional. Aggregation for text columns.
#'       \item startFromSeriesMinDatetime. logical. Optional. Whether to use per series start date.
#'       \item endToSeriesMaxDatetime. logical. Optional. Whether to use per series end date.
#'     }
#'   \item query. character. The text of the generated Spark SQL query.
#' }
#' @examples
#' \dontrun{
#' generatorType <- "TimeSeries"
#' dataset <- list(alias = "my_dataset", datasetId = "59a5af20c80891534e3c2bde")
#' generatorSettings <- list(
#'   datetimePartitionColumn = "date",
#'   timeUnit = "DAY",
#'   timeStep = 1,
#'   defaultNumericAggregationMethod = "sum",
#'   defaultCategoricalAggregationMethod = "mostFrequent",
#'   target = "y"
#' )
#' queryGenerator <- CreateDataEngineQueryGenerator(generatorType, dataset, generatorSettings)
#' }
#' @family Data Engine Query Generator functions
#' @md
#' @export
CreateDataEngineQueryGenerator <- function(generatorType, dataset, generatorSettings) {
  body <- list(
    generatorType = generatorType,
    datasets = list(dataset),
    generatorSettings = generatorSettings
  )
  routeString <- UrlJoin("dataEngineQueryGenerators")
  postResponse <- DataRobotPOST(routeString,
    body = body,
    addUrl = TRUE, returnRawResponse = TRUE, encode = "json"
  )
  response <- WaitForAsyncReturn(GetRedirectFromResponse(postResponse),
    addUrl = FALSE
  )
  queryGenerator <- as.dataRobotQueryGenerator(response)
  return(queryGenerator)
}

#' Retrieve a DataEngineQueryGenerator for time series data prep.
#'
#' @param generatorId character. The Id of the query generator to retrieve.

#' @return A query generator object:
#' \itemize{
#'   \item Id. character. The ID of the created query generator.
#'   \item datasets. list of objects containing elements
#'     \itemize{
#'       \item alias. character. The alias for the dataset used in the query.
#'       \item datasetId. character. The ID of the dataset.
#'       \item datasetVersionId. character. The ID of the dataset version.
#'     }
#'   \item generatorType. character. The type of the query generator.
#'   \item generatorSettings. object containing elements
#'     \itemize{
#'       \item datetimePartitionColumn. character. The name of the date column.
#'       \item timeUnit. character. The unit of the time step.
#'       \item timeStep. integer. The number of units in the time step.
#'       \item defaultNumericAggregationMethod. character. Aggregation for numeric columns.
#'       \item defaultCategoricalAggregationMethod. character. Aggregation for categorical columns.
#'       \item target. character. The name of the target column.
#'       \item multiseriesIdColumns. list of character. Optional. The name of the series id column.
#'       \item defaultTextAggregationMethod. character. Optional. Aggregation for text columns.
#'       \item startFromSeriesMinDatetime. logical. Optional. Whether to use per series start date.
#'       \item endToSeriesMaxDatetime. logical. Optional. Whether to use per series end date.
#'     }
#'   \item query. character. The text of the generated Spark SQL query.
#' }
#' @examples
#' \dontrun{
#' generatorId <- "59a5af20c80891534e3c2bde"
#' queryGenerator <- GetDataEngineQueryGenerator(generatorId)
#' }
#' @family Data Engine Query Generator functions
#' @md
#' @export
GetDataEngineQueryGenerator <- function(generatorId) {
  routeString <- UrlJoin("dataEngineQueryGenerators", generatorId)
  queryGenerator <- DataRobotGET(routeString, simplifyDataFrame = FALSE)
  return(as.dataRobotQueryGenerator(queryGenerator))
}


as.dataRobotDataset <- function(inList) {
  outList <- inList
  if (!is.null(outList$creationDate)) {
    outList$creationDate <- ParseRFC3339Timestamp(outList$creationDate)
  }
  if (!is.null(outList$eda1ModificationDate)) {
    outList$eda1ModificationDate <- ParseRFC3339Timestamp(outList$eda1ModificationDate)
  }
  if (!is.null(outList$lastModificationDate)) {
    outList$lastModificationDate <- ParseRFC3339Timestamp(outList$lastModificationDate)
  }
  class(outList) <- "dataRobotDataset"
  outList
}


#' Create a Dataset from a DataEngineQueryGenerator for time series data prep.
#'
#' @param generatorId character. The ID of the query generator to use.
#' @param datasetId character. Optional. The ID of the dataset to apply the query to.
#' @param datasetVersionId character. Optional. The ID of the dataset version to apply the query to.

#' @return A dataset object:
#' \itemize{
#'   \item datasetId. character. The ID of the created dataset.
#'   \item versionId. character. The ID of the created dataset version.
#'   \item name. character. The name of the created dataset.
#'   \item categories. list of character. The categories of the created dataset.
#'   \item creationDate. POSIXct. The timestamp the dataset was created.
#'   \item createdBy. character. The user who created the dataset.
#'   \item dataPersisted. logical. Whether the dataset has extended data profile available.
#'   \item isDataEngineEligible. logical. Whether the dataset is eligible for data engine queries.
#'   \item isLatestVersion. logical. Whether the dataset is the latest version.
#'   \item isSnapshot. logical. Whether the dataset is a static snapshot.
#'   \item datasetSize. numeric. Size of the dataset as a CSV in bytes.
#'   \item rowCount. numeric. The number of rows in the dataset.
#'   \item processingState. character. The current ingestion process state of the dataset.
#' }
#' @examples
#' \dontrun{
#' generatorId <- "59a5af20c80891534e3c2bde"
#' dataset <- CreateDatasetFromDataEngineQueryGenerator(generatorId)
#' }
#' @family Data Engine Query Generator functions
#' @md
#' @export
CreateDatasetFromDataEngineQueryGenerator <- function(generatorId,
                                                      datasetId = NULL,
                                                      datasetVersionId = NULL) {
  body <- list(queryGeneratorId = generatorId)
  if (!is.null(datasetId)) {
    body$datasetId <- datasetId
  }
  if (!is.null(datasetVersionId)) {
    body$datasetVersionId <- datasetVersionId
  }
  routeString <- UrlJoin("dataEngineWorkspaceStates", "fromDataEngineQueryGenerator")
  postResponse <- DataRobotPOST(routeString, body = body)

  body <- list(workspaceStateId = postResponse$workspaceStateId)
  routeString <- UrlJoin("datasets", "fromDataEngineWorkspaceState")
  postResponse <- DataRobotPOST(routeString,
    body = body,
    addUrl = TRUE, returnRawResponse = TRUE
  )
  response <- WaitForAsyncReturn(GetRedirectFromResponse(postResponse),
    addUrl = FALSE
  )
  dataset <- as.dataRobotDataset(response)
  return(dataset)
}
