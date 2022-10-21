#' Create a new featurelist in a DataRobot project
#'
#' This function allows the user to create a new featurelist
#' in a project by specifying its name and a list of variables
#' to be included
#'
#' DataRobot featurelists define the variables from the modeling
#' dataset used in fitting each project model. Some functions
#' (SetTarget, StartNewAutopilot) optionally accept a featurelist
#' (and use a default featurelist if none is specified).
#'
#' @inheritParams DeleteProject
#' @param listName character. String identifying the new featurelist
#' to be created.
#' @param featureNames character. Vector listing the names of the
#' variables to be included in the featurelist.
#' @return A list with the following four elements describing
#' the featurelist created:
#' \describe{
#'   \item{featurelistId}{Character string giving the unique
#'   alphanumeric identifier for the new featurelist.}
#'   \item{projectId}{Character string giving the projectId
#'   identifying the project to which the featurelist was added.}
#'   \item{features}{Character vector with the names of the
#'   variables included in the new featurelist.}
#'   \item{name}{Character string giving the name of the new
#'   featurelist.}
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   CreateFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2", "otherFeature"))
#' }
#' @export
CreateFeaturelist <- function(project, listName, featureNames) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "featurelists")
  # I(featureNames) tells httr/jsonlite not to unbox length-1 vectors to scalars
  body <- list(name = listName, features = I(featureNames))
  postResponse <- DataRobotPOST(routeString,
                             body = body,
                             returnRawResponse = TRUE,
                             encode = "json")
  featurelistInfo <- DataRobotGET(GetRedirectFromResponse(postResponse), addUrl = FALSE)
  idIndex <- which(names(featurelistInfo) == "id")
  names(featurelistInfo)[idIndex] <- "featurelistId"
  message(paste("Featurelist", listName, "created"))
  as.dataRobotFeaturelist(featurelistInfo)
}


#' This function allows the user to create a new featurelist
#' in a project by specifying its name and a list of variables
#' to be included
#'
#' In time series projects, a new set of modeling features is created after setting the
#' partitioning options. These features are automatically derived from those in the project's
#' dataset and are the features used for modeling. Modeling features are only accessible once
#' the target and partitioning options have been set. In projects that don't use time series
#' modeling, once the target has been set, ModelingFeaturelists and Featurelists will behave
#' the same.
#'
#' @inheritParams CreateFeaturelist
#' @inherit CreateModelingFeaturelist return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   CreateModelingFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#' }
#' @export
CreateModelingFeaturelist <- function(project, listName, featureNames) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelingFeaturelists")
  # I(featureNames) tells httr/jsonlite not to unbox length-1 vectors to scalars
  body <- list(name = listName, features = I(featureNames))
  featurelistInfo <- DataRobotPOST(routeString, body = body, encode = "json")
  featurelistInfo$featurelistId <- featurelistInfo$id
  featurelistInfo$id <- NULL
  message(paste("Featurelist", listName, "created"))
  as.dataRobotFeaturelist(featurelistInfo)
}


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
  if (!is.null(listName)) { body$name <- listName }
  if (!is.null(description)) { body$description <- description }
  if (identical(body, list())) { return(featurelist) } # Nothing to update.
  DataRobotPATCH(routeString,
                 body = body,
                 returnRawResponse = TRUE,
                 encode = "json")
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
  if (!is.null(listName)) { body$name <- listName }
  if (!is.null(description)) { body$description <- description }
  if (identical(body, list())) { return(featurelist) } # Nothing to update.
  DataRobotPATCH(routeString,
                 body = body,
                 returnRawResponse = TRUE,
                 encode = "json")
  GetModelingFeaturelist(projectId, featurelistId)
}


#' Retrieve a specific featurelist from a DataRobot project
#'
#' This function returns information about and the contents
#' of a specified featurelist from a specified project.
#'
#' DataRobot featurelists define the variables from the modeling
#' dataset used in fitting each project model. In most cases,
#' the same featurelist is used in fitting all project models,
#' but models can be fit using alternative featurelists using the
#' RequestNewModel function. To do this, featurelistId is required,
#' and this is one of the elements returned by the GetFeaturelist
#' function.
#'
#' DataRobot featurelists define the variables from the modeling
#' dataset used in fitting each project model. In most cases, the
#' same featurelist is used in fitting all project models, but models
#' can be fit using alternative featurelists using the RequestNewModel
#' function. To do this, featurelistId is required, and this is one of
#' the elements returned by the GetFeaturelist function.
#'
#' @inheritParams DeleteProject
#' @param featurelistId Unique alphanumeric identifier for the featurelist
#' to be retrieved.
#' @return A list with the following elements describing the requested featurelist:
#' \itemize{
#'   \item featurelistId character. The unique alphanumeric identifier for the featurelist.
#'   \item projectId character. The project to which the featurelist belongs.
#'   \item features character. The names of the variables included in the featurelist.
#'   \item name character. The name of the featurelist.
#'   \item created character. A timestamp of when the featurelist was created.
#'   \item isUserCreated logical. Whether or not the featurelist was created by a user
#'     (as opposed to DataRobot automation).
#'   \item numModels numeric. The number of models that currently use this featurelist.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   featureList <- CreateFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#'   featurelistId <- featureList$featurelistId
#'   GetFeaturelist(projectId, featurelistId)
#' }
#' @export
GetFeaturelist <- function(project, featurelistId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "featurelists", featurelistId)
  featurelist <- DataRobotGET(routeString)
  idIndex <- which(names(featurelist) == "id")
  names(featurelist)[idIndex] <- "featurelistId"
  as.dataRobotFeaturelist(featurelist)
}


#' Retrieve a specific modeling featurelist from a DataRobot project
#'
#' In time series projects, a new set of modeling features is created after setting the
#' partitioning options. These features are automatically derived from those in the project's
#' dataset and are the features used for modeling. Modeling features are only accessible once
#' the target and partitioning options have been set. In projects that don't use time series
#' modeling, once the target has been set, ModelingFeaturelists and Featurelists will behave
#' the same.
#'
#' @inheritParams GetFeaturelist
#' @inherit GetFeaturelist return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   featureList <- CreateModelingFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#'   featurelistId <- featureList$featurelistId
#'   GetModelingFeaturelist(projectId, featurelistId)
#' }
#' @export
GetModelingFeaturelist <- function(project, featurelistId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelingFeaturelists", featurelistId)
  featurelist <- DataRobotGET(routeString)
  idIndex <- which(names(featurelist) == "id")
  names(featurelist)[idIndex] <- "featurelistId"
  as.dataRobotFeaturelist(featurelist)
}


as.dataRobotFeaturelist <- function(inList) {
  elements <- c("featurelistId",
                "projectId",
                "features",
                "created",
                "isUserCreated",
                "numModels",
                "name",
                "description")
  ApplySchema(inList, elements)
}


#' Retrieve all featurelists associated with a project
#'
#' This function returns an S3 object of class listOfFeaturelists that
#' describes all featurelists (i.e., lists of modeling variables)
#' available for the project specified by the project parameter.
#' This list may be converted to a dataframe with the as.data.frame
#' method for objects of class listOfFeaturelists.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class 'listOfFeaturelists', which is a
#' list of dataframes: each element of the list corresponds to one
#' featurelist associated with the project, and each dataframe has
#' one row and the following four columns:
#' \itemize{
#'   \item featurelistId. Unique alphanumeric identifier for the featurelist.
#'   \item projectId. Unique alphanumeric project identifier.
#'   \item features. Comma-separated character string listing the variables included in the
#'     featurelist.
#'   \item name. Character string giving the name of the featurelist.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListFeaturelists(projectId)
#' }
#' @export
ListFeaturelists <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "featurelists")
  response <- DataRobotGET(routeString)
  featurelists <- list()
  for (i in seq(nrow(response))) {
    flist <- as.list(response[i, ])
    flist$features <- flist$features[[1]]
    flist$featurelistId <- flist$id
    flist$id <- NULL
    flist <- as.dataRobotFeaturelist(flist)
    featurelists <- append(featurelists, list(flist))
  }
  class(featurelists) <- c("listOfFeaturelists", "listSubclass")
  featurelists
}


#' Retrieve all modeling featurelists associated with a project
#'
#' In time series projects, a new set of modeling features is created after setting the
#' partitioning options. These features are automatically derived from those in the project's
#' dataset and are the features used for modeling. Modeling features are only accessible once
#' the target and partitioning options have been set. In projects that don't use time series
#' modeling, once the target has been set, ModelingFeaturelists and Featurelists will behave
#' the same.
#'
#' @inheritParams ListFeaturelists
#' @inherit ListFeaturelists return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListModelingFeaturelists(projectId)
#' }
#' @export
ListModelingFeaturelists <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelingFeaturelists")
  response <- DataRobotGET(routeString)
  response <- GetServerDataInRows(response)
  featurelists <- list()
  for (i in seq(nrow(response))) {
    flist <- as.list(response[i, ])
    flist$features <- flist$features[[1]]
    flist$featurelistId <- flist$id
    flist$id <- NULL
    flist <- as.dataRobotFeaturelist(flist)
    featurelists <- append(featurelists, list(flist))
  }
  class(featurelists) <- c("listOfModelingFeaturelists", "listSubclass")
  featurelists
}


#' Delete a featurelist
#'
#' @param featurelist list. The featurelist to delete.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   featureList <- CreateFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#'   DeleteFeaturelist(featurelist)
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
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   featureList <- CreateModelingFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#'   featurelistId <- featureList$featurelistId
#'   GetModelingFeaturelist(projectId, featurelistId)
#'   DeleteModelingFeaturelist(projectId, featurelistId)
#' }
#' @export
DeleteModelingFeaturelist <- function(featurelist) {
  projectId <- ValidateProject(featurelist$projectId)
  featurelistId <- featurelist$featurelistId
  routeString <- UrlJoin("projects", projectId, "modelingFeaturelists", featurelistId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}
