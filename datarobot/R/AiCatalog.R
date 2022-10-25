# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' DownloadDatasetAsCsv
#'
#' Downloads the dataset CSV file corresponding to catalogId
#'
#' To Note: While using this method a call to httr::content() requires the installation of the readr package
#'
#' @seealso \link[readr]{https://cran.r-project.org/web/packages/readr/index.html}
#'
#' @param catalogId character. The id of the catalog item.
#'
#' @family AiCatalog
#' @export
DownloadDatasetAsCsv <- function(catalogId) {
  routeString <- UrlJoin("datasets", catalogId, "file")
  response <- DataRobotGET(routeString, returnRawResponse = TRUE)
  csvFile <- httr::content(response)
  return(csvFile)
}
