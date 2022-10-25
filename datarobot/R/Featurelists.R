# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Update a featurelist
#'
#' Updates a featurelist to change the name or description.
#'
#' @param description character. A user-friendly description to give a featurelist.
#' @inheritParams DeleteFeaturelist
#' @inheritParams CreateFeaturelist
#' @inherit CreateFeaturelist return
#' @export
UpdateFeaturelist <- function(featurelist, listName = NULL, description = NULL) {
  projectId <- ValidateProject(featurelist$projectId)
  featurelistId <- featurelist$featurelistId
  routeString <- UrlJoin("projects", projectId, "featurelists", featurelistId)
  body <- list()
  if (!is.null(listName)) {
    body$name <- listName
  }
  if (!is.null(description)) {
    body$description <- description
  }
  if (identical(body, list())) {
    return(featurelist)
  } # Nothing to update.
  DataRobotPATCH(routeString,
    body = body,
    returnRawResponse = TRUE,
    encode = "json"
  )
  GetFeaturelist(projectId, featurelistId)
}


#' Update a modeling featurelist
#'
#' Updates a modeling featurelist to change the name or description.
#'
#' @param description character. A user-friendly description to give a featurelist.
#' @inheritParams DeleteModelingFeaturelist
#' @inheritParams CreateModelingFeaturelist
#' @inherit CreateModelingFeaturelist return
#' @export
UpdateModelingFeaturelist <- function(featurelist, listName = NULL, description = NULL) {
  projectId <- ValidateProject(featurelist$projectId)
  featurelistId <- featurelist$featurelistId
  routeString <- UrlJoin("projects", projectId, "modelingFeaturelists", featurelistId)
  body <- list()
  if (!is.null(listName)) {
    body$name <- listName
  }
  if (!is.null(description)) {
    body$description <- description
  }
  if (identical(body, list())) {
    return(featurelist)
  } # Nothing to update.
  DataRobotPATCH(routeString,
    body = body,
    returnRawResponse = TRUE,
    encode = "json"
  )
  GetModelingFeaturelist(projectId, featurelistId)
}

as.dataRobotFeaturelist <- function(inList) {
  outList <- inList
  idIndex <- which(names(outList) == "id")
  names(outList)[idIndex] <- "featurelistId"
  outList
}

#' Delete a featurelist
#'
#' @param featurelist list. The featurelist to delete.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' featureList <- CreateFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#' DeleteFeaturelist(featurelist)
#' }
#' @export
DeleteFeaturelist <- function(featurelist) {
  projectId <- ValidateProject(featurelist$projectId)
  featurelistId <- featurelist$featurelistId
  routeString <- UrlJoin("projects", projectId, "featurelists", featurelistId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}


#' Delete a modeling featurelist
#'
#' @param featurelist list. The modeling featurelist to delete.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' featureList <- CreateModelingFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#' featurelistId <- featureList$featurelistId
#' GetModelingFeaturelist(projectId, featurelistId)
#' DeleteModelingFeaturelist(projectId, featurelistId)
#' }
#' @export
DeleteModelingFeaturelist <- function(featurelist) {
  projectId <- ValidateProject(featurelist$projectId)
  featurelistId <- featurelist$featurelistId
  routeString <- UrlJoin("projects", projectId, "modelingFeaturelists", featurelistId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}
