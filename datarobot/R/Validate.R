#' Checks if an id is a valid DataRobot ID (24 character string)
#'
#' @param id character. An ID to test whether it is a valid DataRobot ID.
IsId <- function(id) {
  is.character(id) && length(id) == 1 && nchar(id) == 24
}


#' Get a projectId from a project object.
#'
#' @param project object. Either list with projectId element or projectId value
ValidateProject <- function(project) {
  if (is.list(project)) {
    projectId <- project$projectId
  } else {
    projectId <- project
  }
  if (is.null(projectId)) {
    rawMsg <- paste("Project specification does not contain a valid project in call to",
                    capture.output(sys.calls()[[1]][[1]]))
    stop(rawMsg, call. = FALSE)
  } else {
    projectId
  }
}


#' Validate that model belongs to class 'dataRobotModel' and includes
#' projectId and modelId.
#'
#' @param model An S3 object of class dataRobotModel like that returned by
#'   the function GetModel, or each element of the list returned by
#'   the function ListModels.
ValidateModel <- function(model) {
  errorMessage <- "Invalid model specification"
  if (!(is(model, "dataRobotModel") | is(model, "dataRobotFrozenModel") |
        is(model, "dataRobotDatetimeModel") | is(model, "dataRobotPrimeModel"))) {
    stop(errorMessage)
  } else {
    projectId <- model$projectId
    modelId <- model$modelId
    if (IsId(projectId) && IsId(modelId)) {
      model
    } else {
      stop(errorMessage, call. = FALSE)
    }
  }
}


#' Get a calendar id from a calendar object.
#'
#' @param calendar object. Either list with calendarId element or calendarId value
ValidateCalendar <- function(calendar) {
  if (is.list(calendar)) {
    calendarId <- calendar$id
  } else {
    calendarId <- calendar
  }
  if (!IsId(calendarId)) {
    rawMsg <- paste("Calendar specification does not contain a valid project
                    in call to ", sys.calls()[[1]][[1]])
    stop(strwrap(rawMsg), call. = FALSE)
  } else {
    calendarId
  }
}


#' Check if a parameter is in a list of possibilities.
#'
#' @param paramValue object. The parameter value to check.
#' @param paramPossibilities vector. A vector of possible values for the parameter.
#' @param allowNULL logical. Whether or not to allow NULL as a possibility.
#' @param paramName character. The name of the parameter to check.
#' @return TRUE if \code{paramValue} is valid, otherwise returns an error message.
#' @examples
#' \dontrun{
#'   IsParameterIn("all", DataSubset)
#' }
IsParameterIn <- function(paramValue, paramPossibilities, allowNULL = TRUE, paramName = NULL) {
  if (is.null(paramName)) {
    paramName <- substitute(paramPossibilities)
    if (length(paramName) > 1) { paramName <- "value" }
  }
  isUnallowedNull <- is.null(paramValue) && !allowNULL
  if (isUnallowedNull ||
      length(paramValue) > 1 ||
      !(is.null(paramValue) ||
      any(paramValue %in% paramPossibilities))) {
        paste0("Invalid ", sQuote(paramName), ". Must be in ",
               paste(vapply(paramPossibilities, sQuote, character(1)), collapse = ", "),
               " but got ",
               sQuote(gsub("\"", "", capture.output(dput(paramValue)))), " instead.")
  } else { TRUE }
}

#' Ensure a parameter is valid
#'
#' A valid parameter \code{paramValue} is either NULL or in the space
#' of \code{paramPossibilities}.
#'
#' @inheritParams IsParameterIn
#' @return TRUE if \code{paramValue} is valid, otherwise it raises an error.
#' @examples
#' \dontrun{
#'   ValidateParameterIn("all", DataSubset)
#' }
ValidateParameterIn <- function(paramValue, paramPossibilities, allowNULL = TRUE) {
  paramName <- substitute(paramPossibilities)
  if (length(paramName) > 1) { paramName <- "value" }
  error <- IsParameterIn(paramValue,
                         paramPossibilities,
                         allowNULL = allowNULL,
                         paramName = paramName)
  if (isTRUE(error)) { TRUE } else { stop(error) }
}


#' Checks if a partition is valid.
#'
#' @param validationType character. The type of partition to validate.
#' @param partition partition. The partition object.
#' @param reps numeric. The number of repetitions for a CV validation.
#' @param validationPct numeric. The size of the validation set for TVH validation.
ValidatePartition <- function(validationType, partition, reps = NULL, validationPct = NULL) {
  if (identical(validationType, "CV")) {
    if (is.null(reps)) {
      stop(strwrap("Parameter reps must be specified for partition with
              validationType = 'CV'"))
    } else {
      partition$reps <- reps
    }
  } else if (identical(validationType, "TVH")) {
    if (is.null(validationPct)) {
      stop(strwrap("Parameter validationPct must be specified for
                partition with validationType = 'TVH'"))
    } else {
      partition$validationPct <- validationPct
    }
  } else {
    stop(strwrap(paste("validationType", validationType, "not valid")))
  }
  class(partition) <- "partition"
  partition
}

#' Validate that the actuals are a dataframe and contain required columns.
#'
#' @param actuals dataframe. Contains all actuals to be submitted.
#' @param error logical. Should an error be raised if there is an issue?
#' @return TRUE if the actuals dataframe has required properties, otherwise FALSE or raises error.
ValidateActuals <- function(actuals, error = TRUE) {
  errorMsg <- NULL
  if (!is.data.frame(actuals)) {
    errorMsg <- "Actuals is not a dataframe."
  }
  else if (!"associationId" %in% names(actuals)) {
    errorMsg <- "Actuals does not contain associationId key."
  }
  else if (max(nchar(actuals[["associationId"]])) > 128) {
    errorMsg <- "Cannot have associationIds with length over max of 128 characters."
  }
  else if (!is.character(actuals[["associationId"]])) {
    errorMsg <- "AssociationIds must be strings."
  }
  else if (!"actualValue" %in% names(actuals)) {
    errorMsg <- "Actuals does not contain actualValue key."
  }
  else if ("wasActedOn" %in% names(actuals) && !is.logical(actuals[["wasActedOn"]])) {
    errorMsg <- "Optional key wasActedOn must be logical."
  }
  else if ("timestamp" %in% names(actuals) && !inherits(actuals[["timestamp"]], "POSIXt")) {
    errorMsg <- "Optional key timestamp must inherit from POSIXt."
  }
  if (!is.null(errorMsg)) {
    if (isTRUE(error)) { stop(errorMsg) } else { FALSE }
  } else {
    TRUE
  }
}


#' Validate that the multiseries properties indicate a successful multiseries setup.
#'
#' @param properties list. List of multiseries properties.
#' @param error logical. Should an error be raised if there is an issue?
#' @return TRUE if all properties verify, otherwise FALSE or raises error.
#' @export
ValidateMultiSeriesProperties <- function(properties, error = TRUE) {
  errorMsg <- NULL
  if (!is.list(properties)) {
    errorMsg <- "Properties are not a list."
  }
  else if (!("timeSeriesEligible" %in% names(properties))) {
    errorMsg <- "Properties do not contain timeSeriesEligible key."
  }
  else if (!("crossSeriesEligible" %in% names(properties))) {
    errorMsg <- "Properties do not contain crossSeriesEligible key."
  }
  else if (!isTRUE(properties$timeSeriesEligible)) {
    errorMsg <- paste("The selected datetime partition and multiseries id columns are not eligible",
                      "for time series modeling, i.e. they are insufficiently unique or regular.")
  }
  else if (!isTRUE(properties$crossSeriesEligible) && !is.null(properties$crossSeriesEligible)) {
    errorMsg <- paste("The selected cross-series group-by column is not eligible for",
                      "cross series modeling.")
  }
  if (!is.null(errorMsg)) {
    if (isTRUE(error)) { stop(errorMsg) } else { FALSE }
  } else {
    TRUE
  }
}
