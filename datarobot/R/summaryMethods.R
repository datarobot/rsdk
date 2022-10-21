#' DataRobot S3 object methods for R's generic summary function
#'
#' These functions extend R's generic summary function to the
#' DataRobot S3 object classes dataRobotModel, dataRobotProject,
#' listOfBlueprints, listOfFeaturelists, listOfModels, and
#' projectSummaryList.
#'
#' @param object The S3 object to be summarized.
#' @param nList integer. For the 'listOf' class objects, the first
#'   nList elements of the list are summarized in the dataframe in
#'   the second element of the list returned by the function.
#' @param \dots list. Not currently used.
#' @return An object-specific summary: for objects of class
#'   dataRobotModel and dataRobotProject, this summary is a
#'   character vector giving key characteristics of the model or
#'   project, respectively; for the other object classes, the
#'   value is a two-element list where the first element is a
#'   brief summary character string and the second element
#'   is a more detailed dataframe with nList elements.
#'   The summary of object has the following components:
#'   modelType, expandedModel (constructed from modelType and processes),
#'   modelId, blueprintId, and projectId.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   summary(model)
#' }
#' @export
summary.dataRobotModel <- function(object, ...) {
  components <- union(object$processes, object$modelType)
  expandedModel <- paste(components, collapse = "::")
  c(modelType = object$modelType, expandedModel = expandedModel,
    modelId = object$modelId, blueprintId = object$blueprintId,
    projectId = object$projectId)
}


#' @rdname summary.dataRobotModel
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   project <- GetProject(projectId)
#'   summary(project)
#' }
#' @export
summary.dataRobotProject <- function(object, ...) {
  c(projectName = object$projectName,
    projectId = object$projectId,
    created = object$created, fileName = object$fileName,
    target = object$target, targetType = object$targetType,
    metric = object$metric)
}

#' @rdname summary.dataRobotModel
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   blueprints <- ListBlueprints(projectId)
#'   summary(blueprints)
#' }
#' @export
summary.listOfBlueprints <- function(object, nList = 6, ...) {
  nList <- min(nList, length(object))
  if (nList <= 0) { return(list()) }
  #  Each element of a listOfBlueprints object has 4 components:
  #                             projectId, processes, blueprintId, and modelType
  #
  #  The first element of the summary list returned gives the total number of
  #  elements in object and, if projectId is constant, this value is also
  #  displayed; otherwise, an information message is displayed indicating
  #  that object includes blueprints from different projects
  #
  #  The format of the second summary list element also depends on whether
  #  projectId is unique: if so, the common value of projectId is included in
  #  the first summary list element and not repeated in the second list element;
  #  if not, projectId is included in the dataframe returned in the second
  #  summary list element.  To simplify the code, the required values of the
  #  non-projectId list elements that are always required are generated first
  sumFrame <- NULL
  for (i in 1:nList) {
    modelType <- object[[i]]$modelType
    components <- union(modelType, object[[i]]$processes)
    expandedModel <- paste(components, collapse = "::")
    blueprintId <- object[[i]]$blueprintId
    projectId <- object[[i]]$projectId
    upFrame <- data.frame(projectId = projectId, modelType = modelType,
                          expandedModel = expandedModel,
                          blueprintId = blueprintId,
                          stringsAsFactors = FALSE)
    sumFrame <- rbind.data.frame(sumFrame, upFrame)
  }
  nBlue <- length(object)
  projectId <- object[[1]]$projectId
  getProjectIdFunction <- function(xList) { xList$projectId }
  nProjectId <- length(unique(unlist(lapply(object, getProjectIdFunction))))
  if (nProjectId == 1) {
    firstElement <- paste("First", nList, "of", nBlue,
                          "blueprints for projectId", projectId)
    secondElement <- sumFrame[, 2:4]
  } else {
    firstElement <- paste("First", nList, "of", nBlue,
                          "blueprints from a mixed list of",
                          nProjectId, "projects")
    secondElement <- sumFrame
  }
  list(generalSummary = firstElement,
       detailedSummary = secondElement)
}

#' @rdname summary.dataRobotModel
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   featureList <- CreateFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#'   summary(featureList)
#' }
#' @export
summary.listOfFeaturelists <- function(object, nList = 6, ...) {
  nList <- min(nList, length(object))
  if (nList <= 0) { return(list()) }
  #  Each element of a listOfFeaturelists object has 4 components:
  #                            featurelistId, projectId, features, and name
  #
  #  The first element of the summary list returned gives the total number of
  #  elements in object and, if projectId is constant, this value is also
  #  displayed; otherwise, an information message is displayed indicating
  #  that object includes featurelists from different projects
  #
  #  The format of the second summary list element also depends on whether
  #  projectId is unique: if so, the common value of projectId is included
  #  in the first summary list element and not repeated in the second list
  #  element; if not, projectId is included in the dataframe returned in
  #  the second summary list element.  To simplify the code, the required
  #  values of the non-projectId list elements that are always required
  #  are generated first
  #
  sumFrame <- NULL
  for (i in 1:nList) {
    upFrame <- as.data.frame(object[[i]])
    sumFrame <- rbind.data.frame(sumFrame, upFrame)
  }
  nf <- length(object)
  projectId <- object[[1]]$projectId
  getProjectIdFunction <- function(xList) { xList$projectId }
  nProjectId <- length(unique(unlist(lapply(object, getProjectIdFunction))))
  if (nProjectId == 1) {
    firstElement <- paste("First", nList, "of", nf,
                          "featurelists for projectId", projectId)
    allNames <- colnames(sumFrame)
    keepNames <- setdiff(allNames, "projectId")
    keepCols <- which(allNames %in% keepNames)
    secondElement <- sumFrame[, keepCols]
  } else {
    firstElement <- paste("First", nList, "of", nf,
                          "featurelists from a mixed list of",
                          nProjectId, "projects")
    secondElement <- sumFrame
  }
  list(generalSummary = firstElement,
       detailedSummary = secondElement)
}


#' @rdname summary.dataRobotModel
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   models <- ListModels(projectId)
#'   summary(models)
#' }
#' @export
summary.listOfModels <- function(object, nList = 6, ...) {
  nModels <- length(object)
  nList <- min(nList, nModels)
  if (nList <= 0) { return(list()) }
  sumFrame <- as.data.frame(object, simple = TRUE)
  firstElement <- paste("First", nList, "of", nModels, "models from:",
                        deparse(substitute(object)),
                        "(S3 object of class listOfModels)")
  secondElement <- sumFrame[seq(1, nList, 1), ]
  list(generalSummary = firstElement,
       detailedSummary = secondElement)
}


#' @rdname summary.dataRobotModel
#' @examples
#' \dontrun{
#'   projectSummary <- ListProjects()
#'   summary(projectSummary)
#' }
#' @export
summary.projectSummaryList <- function(object, nList = 6, ...) {
  sumFrame <- as.data.frame(object, simple = TRUE)
  nProjects <- nrow(sumFrame)
  nList <- min(nList, nProjects)
  if (nList <= 0) { return(list()) }
  firstElement <- paste("First", nList, "of", nProjects,
                        "projects from:", deparse(substitute(object)),
                        "(S3 object of class projectSummaryList)")
  secondElement <- sumFrame[seq(1, nList, 1), ]
  list(generalSummary = firstElement,
       detailedSummary = secondElement)
}
