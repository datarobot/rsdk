#' Retrieve information about all DataRobot models with a rating table.
#'
#' @inheritParams DeleteProject
#' @return data.frame containing information about each model with a rating table in a
#'   project (one row per model with a rating table).
#' @examples
#' \dontrun{
#'  projectId <- "5984b4d7100d2b31c1166529"
#'  ListRatingTableModels(projectId)
#' }
#' @export
ListRatingTableModels <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "ratingTableModels")
  ratingTableModels <- DataRobotGET(routeString, simplifyDataFrame = FALSE)
  ratingTableModels <- lapply(ratingTableModels, as.dataRobotRatingTableModel)
  class(ratingTableModels) <- c("listOfRatingTableModels", "listOfModels", "listSubclass")
  ratingTableModels
}


#' Retrieve information about specified model with a rating table.
#'
#' @inheritParams GetModel
#' @return list containing information about specified model with a rating table.
#' @examples
#' \dontrun{
#'  projectId <- "5984b4d7100d2b31c1166529"
#'  modelId <- "5984b4d7100d2b31c1166529"
#'  GetRatingTableModel(projectId, modelId)
#' }
#' @export
GetRatingTableModel <- function(project, modelId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "ratingTableModels", modelId)
  as.dataRobotRatingTableModel(DataRobotGET(routeString))
}

#' Create a new model from a rating table.
#'
#' @inheritParams GetRatingTable
#' @return An integer value that can be used as the modelJobId parameter
#'   in subsequent calls to the GetModelFromJobId function.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ratingTableId <- "5984b4d7100d2b31c1166529"
#'   RequestNewModel(projectId, ratingTableId)
#' }
#' @export
RequestNewRatingTableModel <- function(project, ratingTableId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "ratingTableModels")
  if (is(ratingTableId, "dataRobotRatingTable")) {
    ratingTableId <- ratingTableId$id
  }
  body <- list("ratingTableId" = ratingTableId)
  postResponse <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  message("New model request from rating table received")
  JobIdFromResponse(postResponse)
}


#' Retrieve a new or updated rating table model defined by a job ID.
#'
#' @inheritParams DeleteProject
#' @param ratingTableModelJobId integer. The ID returned by \code{RequestNewRatingTableModel}.
#' @param maxWait integer. The maximum time (in seconds) to wait for the retrieve to complete.
#' @return An S3 object of class 'dataRobotRatingTableModel' summarizing all
#'   available information about the model.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ratingTableId <- "5984b4d7100d2b31c1166529"
#'   ratingTableModelJobId <- RequestNewModel(projectId, ratingTableId)
#'   GetRatingTableModelFromJobId(project, ratingTableModelJobId)
#' }
#' @export
GetRatingTableModelFromJobId <- function(project, ratingTableModelJobId, maxWait = 600) {
  model <- GetModelFromJobId(project, ratingTableModelJobId, maxWait = maxWait)
  GetRatingTableModel(project, model$modelId)
}


#' Retrieve information about all rating tables.
#'
#' @inheritParams DeleteProject
#' @return data.frame containing information about each rating table in a
#'   project (one row per model with a rating table).
#' @examples
#' \dontrun{
#'  projectId <- "5984b4d7100d2b31c1166529"
#'  ListRatingTables(projectId)
#' }
#' @export
ListRatingTables <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "ratingTables")
  ratingTables <- DataRobotGET(routeString, simplifyDataFrame = FALSE)
  ratingTables <- GetServerDataInRows(ratingTables)
  ratingTables <- lapply(ratingTables, as.dataRobotRatingTable)
  class(ratingTables) <- c("listOfRatingTables", "listSubclass")
  ratingTables
}


#' Retrieve a single rating table.
#'
#' @inheritParams DeleteProject
#' @param ratingTableId character. The ID of the rating table.
#' @return An S3 object of class 'dataRobotRatingTable' summarizing all
#'   available information about the rating table.
#' @examples
#' \dontrun{
#'  projectId <- "5984b4d7100d2b31c1166529"
#'  ratingTableId <- "5984b4d7100d2b31c1166529"
#'  GetRatingTable(projectId, ratingTableId)
#' }
#' @export
GetRatingTable <- function(project, ratingTableId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "ratingTables", ratingTableId)
  ratingTable <- as.dataRobotRatingTable(DataRobotGET(routeString))
  WarnOnInvalidRatingTable(ratingTable)
  ratingTable
}


#' Download a rating table to a CSV.
#'
#' @inheritParams GetRatingTable
#' @param filename character. Filename of file to save the rating table to.
#' @return Nothing returned, but downloads the file to the stated filename.
#' @examples
#' \dontrun{
#'  projectId <- "5984b4d7100d2b31c1166529"
#'  ratingTableId <- "5984b4d7100d2b31c1166529"
#'  file <- file.path(tempdir(), "ratingTable.csv")
#'  DownloadRatingTable(projectId, ratingTableId, file)
#' }
#' @export
DownloadRatingTable <- function(project, ratingTableId, filename) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "ratingTables", ratingTableId, "file")
  response <- DataRobotGET(routeString, as = "file", filename = filename)
  invisible(NULL)
}


#' Creates and validates a new rating table from an uploaded CSV.
#'
#' @inheritParams DeleteProject
#' @param parentModelId integer. The id of the model to validate the rating table against.
#' @param dataSource object. Either (a) the name of a CSV file, or (b) a
#'  dataframe. This parameter identifies the source of the rating table.
#' @param ratingTableName character. Optional. The name of the rating table.
#' @return An integer value that can be used as the JobId parameter
#'   in subsequent calls representing this job.
#' @examples
#' \dontrun{
#'    projectId <- "5984b4d7100d2b31c1166529"
#'    modelId <- "5984b4d7100d2b31c1166529"
#'    CreateRatingTable(projectId, modelId, dataSource = "myRatingTable.csv")
#' }
#' @export
CreateRatingTable <- function(project, parentModelId, dataSource,
                              ratingTableName = "Uploaded Rating Table") {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "ratingTables")
  body <- list(parentModelId = parentModelId,
               ratingTableName = ratingTableName,
               ratingTableFile = UploadData(dataSource))
  postResponse <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  JobIdFromResponse(postResponse)
}

#' Renames a rating table to a different name.
#'
#' @inheritParams GetRatingTable
#' @param ratingTableName character. The new name for the rating table.
#' @return An S3 object of class 'dataRobotRatingTable' summarizing all
#'   available information about the renamed rating table.
#' @examples
#' \dontrun{
#'   projectId <- "5984b4d7100d2b31c1166529"
#'   ratingTableId <- "5984b4d7100d2b31c1166529"
#'   RenameRatingTable(projectId, ratingTableId, "Renamed Table")
#' }
#' @export
RenameRatingTable <- function(project, ratingTableId, ratingTableName) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "ratingTables", ratingTableId)
  body <- list(ratingTableName = ratingTableName)
  ratingTable <- as.dataRobotRatingTable(DataRobotPATCH(routeString, body = body))
  WarnOnInvalidRatingTable(ratingTable)
  ratingTable
}


#' Get a rating table from the rating table job metadata.
#'
#' @inheritParams DeleteProject
#' @param ratingTableJobId integer. The job ID returned by \code{CreateRatingTable}.
#' @param maxWait integer. The maximum time (in seconds) to wait for the retrieve to complete.
#' @return An S3 object of class 'dataRobotRatingTable' summarizing all
#'   available information about the rating table.
#' @examples
#' \dontrun{
#'    projectId <- "5984b4d7100d2b31c1166529"
#'    modelId <- "5984b4d7100d2b31c1166529"
#'    ratingTableJobId <- CreateRatingTable(projectId, modelId, dataSource = "myRatingTable.csv")
#'    GetRatingTableFromJobId(projectId, ratingTableJobId)
#' }
#' @export
GetRatingTableFromJobId <- function(project, ratingTableJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", ratingTableJobId)
  ratingTable <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                    failureStatuses = JobFailureStatuses)
  ratingTable <- as.dataRobotRatingTable(ratingTable)
  WarnOnInvalidRatingTable(ratingTable)
  ratingTable
}


GetRatingTableValidationError <- function(ratingTable) {
  ratingTable$validationError
}
IsValidRatingTable <- function(ratingTable) {
  identical(GetRatingTableValidationError(ratingTable), "")
}
WarnOnInvalidRatingTable <- function(ratingTable) {
  if (!IsValidRatingTable(ratingTable)) {
    warning("The retrieved rating table was invalid, validation error: ",
            GetRatingTableValidationError(ratingTable))
  }
}


as.dataRobotRatingTableModel <- function(inList) {
  elements <- c("featurelistId",
                "processes",
                "featurelistName",
                "projectId",
                "samplePct",
                "isFrozen",
                "modelType",
                "metrics",
                "modelCategory",
                "blueprintId",
                "id",
                "modelNumber",
                "ratingTableId",
                "projectName",
                "projectTarget",
                "projectMetric")
  outList <- ApplySchema(inList, elements)
  class(outList) <- "dataRobotRatingTableModel"
  outList
}


as.dataRobotRatingTable <- function(inList) {
  elements <- c("id", "projectId", "ratingTableName", "originalFilename",
                "parentModelId", "modelId", "modelJobId", "validationJobId",
                "validationError")
  outList <- ApplySchema(inList, elements)
  class(outList) <- "dataRobotRatingTable"
  outList
}
