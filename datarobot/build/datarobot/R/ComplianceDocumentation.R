GetComplianceDocumentationBody <- function(templateId = NULL) {
  if (is(templateId, "dataRobotComplianceDocTemplate")) {
    templateId <- templateId$id
  }
  if (!is.null(templateId)) { list("templateId" = templateId) } else { list() }
}


#' Create compliance documentation from a model.
#'
#' Note that if you're looking to download compliance documentation to a DOCX file, you can
#' call \code{DownloadComplianceDocumentation} directly without using this function.
#'
#' @inheritParams DeleteModel
#' @param templateId character. Optional. The ID of the template to use in generating custom
#'   model documentation.
#' @return An integer value that can be used as the jobId parameter in a subsequent call
#'   to \code{WaitForJobToComplete}.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   jobId <- CreateComplianceDocumentation(model) # optional step
#'   WaitForJobToComplete(projectId, jobId)        # optional step
#'   DownloadComplianceDocumentation(model)
#' }
#' @export
CreateComplianceDocumentation <- function(model, templateId = NULL) {
  model <- ValidateModel(model)
  projectId <- model$projectId
  modelId <- model$modelId
  if (is(templateId, "dataRobotComplianceDocTemplate")) {
    templateId <- templateId$id
  }
  routeString <- UrlJoin("projects", projectId, "models", modelId, "complianceDocs")
  body <- GetComplianceDocumentationBody(templateId)
  postResponse <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  JobIdFromResponse(postResponse)
}


#' Download compliance documentation (in DOCX format).
#'
#' This function will create the compliance documentation first if it has not already been created.
#' To create compliance documentation without downloading it, use
#' \code{CreateComplianceDocumentation}. You can then skip the create step in this function by using
#' `create = FALSE`.
#'
#' @inheritParams CreateComplianceDocumentation
#' @param filename character. Filename of file to save the compliance documentation to.
#' @param create logical. Should we create the compliance documentation prior to downloading?
#' @param maxWait integer. How long to wait (in seconds) for compliance documentation creation
#'   before raising a timeout error? Default 600.
#' @return Nothing returned, but downloads the file to the stated filename.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   DownloadComplianceDocumentation(model)
#' }
#' @export
DownloadComplianceDocumentation <- function(model, filename, templateId = NULL,
                                            create = TRUE, maxWait = 600) {
  model <- ValidateModel(model)
  projectId <- model$projectId
  modelId <- model$modelId
  if (isTRUE(create)) {
    jobId <- CreateComplianceDocumentation(model, templateId = templateId)
    WaitForJobToComplete(projectId, jobId, maxWait = maxWait)
  }
  routeString <- UrlJoin("projects", projectId, "models", modelId, "complianceDocs")
  query <- GetComplianceDocumentationBody(templateId)
  DataRobotGET(routeString, query = query, as = "file", filename = filename)
  invisible(NULL)
}


#' Retrieve information about all compliance doc templates.
#'
#' @param namePart character. Return only compliance doc templates that have a name that contains
#'   this string.
#' @param limit integer. Return only this many compliance doc templates.
#' @param offset integer. Skip this many compliance doc templates before returning.
#' @return list of available compliance doc templates. Contains:
#'  \itemize{
#'    \item name character. The name of the compliance doc template.
#'    \item creatorUsername character. The name of the user who created the compliance doc template.
#'    \item orgId character. The ID of the organization of the creator user.
#'    \item creatorId character. The ID of the creator user.
#'    \item sections list. The list of sections that define the template.
#'    \item id character. The ID of the template.
#'  }
#' @examples
#' \dontrun{
#'  # Get all compliance doc templates
#'  ListComplianceDocTemplates()
#'  Get the first three compliance doc templates with names that contain "foo".
#'  ListComplianceDocTemplates(namePart = "foo", limit = 3)
#' }
#' @export
ListComplianceDocTemplates <- function(namePart = NULL, limit = NULL, offset = NULL) {
  query <- list()
  query$namePart <- namePart
  query$limit <- limit
  query$offset <- offset
  templates <- DataRobotGET("complianceDocTemplates", query = query, simplifyDataFrame = FALSE)
  templates <- GetServerDataInRows(templates)
  templates <- lapply(templates, as.dataRobotComplianceDocTemplate)
  class(templates) <- c("listOfComplianceDocTemplates", "listSubclass")
  templates
}


GetComplianceDocTemplateRoute <- function(templateId = NULL, type = NULL) {
  type <- if (!is.null(templateId)) { templateId } else { "default" }
  UrlJoin("complianceDocTemplates", type)
}

GetComplianceDocTemplateQuery <- function(type = NULL) {
  if (!is.null(type)) { list("type" = type) } else { list() }
}

as.dataRobotComplianceDocTemplate <- function(template) {
  template <- ApplySchema(template,
                          c("id", "creatorId", "creatorUsername", "orgId", "name", "sections"))
  class(template) <- "dataRobotComplianceDocTemplate"
  template
}


#' Get a compliance doc template.
#'
#' A custom compliance doc template can be retrieved using \code{templateId}. Default compliance
#' doc templates that are built-in to DataRobot can be retrieved by using the \code{type}
#' parameter. A type of NULL or "normal" will retrieve the default template. A type of "timeSeries"
#' can be used to retrieve the default time series template.
#'
#' @inheritParams CreateComplianceDocumentation
#' @param type character. Optional. The type of compliance doc to get. Can be "normal" to retrieve
#'   the default template or "timeSeries" to get the default time series template.
#' @return An S3 object of class 'dataRobotComplianceDocTemplate' that contains:
#'  \itemize{
#'    \item name character. The name of the compliance doc template.
#'    \item creatorUsername character. The name of the user who created the compliance doc template.
#'    \item orgId character. The ID of the organization of the creator user.
#'    \item creatorId character. The ID of the creator user.
#'    \item sections list. The list of sections that define the template.
#'    \item id character. The ID of the template.
#'  }
#' @examples
#' \dontrun{
#'   GetComplianceDocTemplate()  # get the default template
#'   GetComplianceDocTemplate(type = "normal")  # get the default template
#'   GetComplianceDocTemplate(type = "timeSeries")  # get the default time series template
#'   templateId <- "5cf85080d9436e5c310c796d"
#'   GetComplianceDocTemplate(templateId) # Get a custom template for a specific ID.
#' }
#' @export
GetComplianceDocTemplate <- function(templateId = NULL, type = NULL) {
  if (is(templateId, "dataRobotComplianceDocTemplate")) {
    templateId <- templateId$id
  }
  routeString <- GetComplianceDocTemplateRoute(templateId = templateId, type = type)
  query <- GetComplianceDocTemplateQuery(type)
  template <- DataRobotGET(routeString, query = query, simplifyDataFrame = FALSE)
  if (is.null(template$name)) { template$name <- "Default" }
  as.dataRobotComplianceDocTemplate(template)
}


#' Download a compliance doc template (in JSON format).
#'
#' @inheritParams GetComplianceDocTemplate
#' @param filename character. Filename of file to save the compliance doc template to.
#' @return Nothing returned, but downloads the file to the stated filename.
#' @examples
#' \dontrun{
#'   DownloadComplianceDocTemplate("template.json")  # download the default template
#'   # download the default template
#'   DownloadComplianceDocTemplate("template.json", type = "normal")
#'   # download the default time series template
#'   DownloadComplianceDocTemplate("template.json" type = "timeSeries")
#'   templateId <- "5cf85080d9436e5c310c796d"
#'   DownloadComplianceDocTemplate(templateId) # Download a custom template for a specific ID.
#' }
#' @export
DownloadComplianceDocTemplate <- function(filename = "template.json", templateId = NULL,
                                          type = NULL) {
  if (is(templateId, "dataRobotComplianceDocTemplate")) {
    templateId <- templateId$id
  }
  routeString <- GetComplianceDocTemplateRoute(templateId = templateId, type = type)
  query <- GetComplianceDocTemplateQuery(type)
  DataRobotGET(routeString, query = query, as = "file", filename = filename)
  invisible(NULL)
}


#' Upload a compliance doc template.
#'
#' The structure of the compliance doc template can be specified by either a file specified by
#' \code{filename} or by specifying it with a list via \code{sections}.
#'
#' @param name character. A name to identify the compliance doc template by.
#' @param filename character. Optional. Filename of file to save the compliance doc template to.
#' @param sections list. Optional. Section definitions for the compliance template.
#' @return Nothing returned, but uploads the compliance doc template.
#' @examples
#' \dontrun{
#'  ## Create a compliance documentation template from uploading a file
#'  DownloadComplianceDocTemplate("template.json")
#'  # Edit template.json in your favorite editor
#'  UploadComplianceDocTemplate("myTemplate", "template.json")
#'
#' ## Create a compliance documentation template from a list.
#' sections <- list(list("title" = "Missing Values Report",
#'                       "highlightedText" = "NOTICE",
#'                       "regularText" = paste("This dataset had a lot of Missing Values.",
#'                                             "See the chart below: {{missingValues}}"),
#'                       "type" = "user"),
#'                  list("title" = "Blueprints",
#'                       "regularText" = "{{blueprintDiagram}} /n Blueprint for this model",
#'                       "type" = "user"))
#' }
#' @export
UploadComplianceDocTemplate <- function(name, filename = NULL, sections = NULL) {
  if (is.null(sections) && is.null(filename)) {
    stop("Must define template with either `filename` or `sections`.")
  }
  if (is.null(sections) && !is.null(filename)) {
    templateData <- jsonlite::fromJSON(filename, simplifyDataFrame = FALSE)
    sections <- templateData$sections
  }
  body <- list(name = name, sections = sections)
  DataRobotPOST("complianceDocTemplates", body = body, encode = "json")
  invisible(NULL)
}


#' Update the name or sections of an existing doc template.
#'
#' Note that default templates cannot be updated.
#'
#' @inheritParams UploadComplianceDocTemplate
#' @param templateId character. The ID of the template to update.
#' @param name character. Optional. A new name to identify the compliance doc template by.
#' @return The updated compliance doc template object.
#' @examples
#' \dontrun{
#' sections <- list(list("title" = "Missing Values Report",
#'                       "highlightedText" = "NOTICE",
#'                       "regularText" = paste("This dataset had a lot of Missing Values."
#'                                             "See the chart below: {{missingValues}}"),
#'                       "type" = "user"),
#'                  list("title" = "Blueprints",
#'                       "regularText" = "{{blueprintDiagram}} /n Blueprint for this model",
#'                       "type" = "user"))
#'   templateId <- "5cf85080d9436e5c310c796d"
#'   UpdateComplianceDocTemplate(templateId, name = "newName", sections = sections)
#' }
#' @export
UpdateComplianceDocTemplate <- function(templateId, name = NULL, sections = NULL) {
  if (is(templateId, "dataRobotComplianceDocTemplate")) {
    templateId <- templateId$id
  }
  routeString <- GetComplianceDocTemplateRoute(templateId = templateId, type = NULL)
  body <- list()
  body$name <- name
  body$sections <- sections
  DataRobotPATCH(routeString, body = body, encode = "json")
  GetComplianceDocTemplate(templateId)
}


#' Deletes a compliance doc template.
#'
#' Note that default templates cannot be deleted.
#'
#' @inheritParams UpdateComplianceDocTemplate
#' @return Nothing returned, but deletes the compliance doc template.
#' @examples
#' \dontrun{
#'   templateId <- "5cf85080d9436e5c310c796d"
#'   DeleteComplianceDocTemplate(templateId)
#' }
#' @export
DeleteComplianceDocTemplate <- function(templateId) {
  if (is(templateId, "dataRobotComplianceDocTemplate")) {
    templateId <- templateId$id
  }
  routeString <- GetComplianceDocTemplateRoute(templateId = templateId, type = NULL)
  DataRobotDELETE(routeString)
  invisible(NULL)
}
