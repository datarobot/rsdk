#' Retrieve data on tuning parameters for a particular model.
#'
#' @param model dataRobotModel. A DataRobot model object to get tuning parameters for.
#' @return A list detailing the following about each tuning parameter:
#' \itemize{
#'   \item currentValue character. The current searched values of that parameter.
#'   \item defaultValue character. The default value of that parameter.
#'   \item parameterId character. A unique ID for that particular parameter.
#'   \item parameterName character. The name of the tuning parameter.
#'   \item taskName character. The name of the task the parameter is for.
#'   \item constraints list. A list describing constraints on the possible values for the parameter.
#'     Will be one of \code{int} or \code{float} specifying a \code{min} and \code{max} value, or
#'     will be \code{select} and will specify possible values from a list of choices. \code{int} and
#'     \code{float} correspond with integer and floating-point parameter spaces respectively. It is
#'     possible for a parameter to be multiple types. Lastly, some parameters will also have a
#'     \code{supportsGridSearch} logical for whether or not that parameter can be grid searched
#'     or not.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   GetTuningParameters(model)
#' }
#' @export
GetTuningParameters <- function(model) {
  model <- ValidateModel(model)
  modelId <- model$modelId
  projectId <- ValidateProject(model$projectId)
  routeString <- UrlJoin("projects", projectId, "models", modelId, "advancedTuning", "parameters")
  params <- DataRobotGET(routeString, simplify = FALSE)
  params <- ApplySchema(params, c("tuningParameters", "tuningDescription"))
  params$tuningParameters <- lapply(params$tuningParameters,
                                    ApplySchema,
                                    schema = c("currentValue", "defaultValue", "parameterId",
                                               "parameterName", "taskName", "constraints"))
  class(params) <- c("listOfDataRobotTuningParameters", "listSubclass")
  params
}


SummarizeConstraints <- function(constraints) {
  processedConstraints <- list()
  if ("int" %in% names(constraints)) {
    processedConstraints <- append(processedConstraints,
                                   paste(constraints$int$min, "to", constraints$int$max))
  }
  if ("float" %in% names(constraints)) {
    processedConstraints <- append(processedConstraints,
                                   paste(constraints$float$min, "to", constraints$float$max))
  }
  if ("select" %in% names(constraints)) {
    processedConstraints <- append(processedConstraints,
                                   paste("select from:",
                                         paste0(constraints$select$values, collapse = ", ")))
  }
  paste0(processedConstraints, collapse = " or ")
}

#' Summarize the list of tuning parameters available for a model.
#'
#' @param object list. The list of tuning parameters to summarize.
#' @param ... list. Extra parameters that are ignored. Used to allow S3 inheritance to work.
#' @return A data.frame detailing the following about each tuning parameter:
#' \itemize{
#'   \item name character. The name of the tuning parameter.
#'   \item current character. The current searched values of that parameter.
#'   \item default character. The default value of that parameter.
#'   \item constraint character. A short description of the possible values that parameter can take.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   summary(GetTuningParameters(model))
#' }
#' @export
summary.listOfDataRobotTuningParameters <- function(object, ...) {
  params <- object
  if ("tuningParameters" %in% names(params)) {
    params <- params$tuningParameters
  }
  paramsSummary <- data.frame()
  for (param in params) {
    paramSummary <- data.frame(name = as.character(param$parameterName),
                               current = as.character(param$currentValue),
                               default = as.character(param$defaultValue),
                               constraint = SummarizeConstraints(param$constraints))
    paramsSummary <- rbind(paramsSummary, paramSummary)
  }
  paramsSummary
}


#' Create a function to initiate hyperparameter tuning for a particular model.
#'
#' The advanced tuning feature allows you to manually set model parameters and override the
#' DataRobot default selections.
#'
#' @inheritParams GetTuningParameters
#' @seealso RunInteractiveTuning
#' @return A function that can be used to tune the model. The function will take \code{model},
#'   the model object to tune, and will have individual arguments for each tunable hyperparameter
#'   that are each set to the default value for that hyperparameter. Furthermore, the function
#'   takes \code{tuningDescription} which can be used to describe the hyperparameter tuning
#'   taking place for future reference. The function itself will return a job ID that can be used
#'   to get the tuned model.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   myXGBModel <- GetModel(projectId, modelId)
#'   RunTune <- StartTuningSession(myXGBModel)
#'   tuningJob <- RunTune(myXGBModel, colsample_bytree = 0.4, colsample_bylevel = 0.8)
#'   tunedModel <- GetModelFromJobId(projectId, tuningJob)
#' }
#' @export
StartTuningSession <- function(model) {
  params <- GetTuningParameters(model)$tuningParameters
  parameterNames <- unlist(lapply(params, `[[`, "parameterName"))
  defaultValues <- unlist(lapply(params, `[[`, "defaultValue"))
  args <- alist(model =) # nolint: infix_spaces_linter.
  args <- append(append(args, defaultValues), "")
  names(args) <- append(append("model", parameterNames), "tuningDescription")

  tuningFunction <- function() {
    sentParams <- list()
    requestedParams <- as.list(sys.call())[c(-1, -2)]
    availableParams <- GetTuningParameters(model)$tuningParameters

    # Force evaluation of params
    findParam <- function(param) {
      paramValue <- try(eval(param), silent = TRUE)
      if (is(paramValue, "try-error")) {
        found <- FALSE
        i <- 0
        while (!isTRUE(found)) {
          paramValue <- try(get(as.character(param), envir = parent.frame(i)), silent = TRUE)
          found <- !is(paramValue, "try-error")
          i <- i + 1
          if (i > 10) { stop("object '", param, "' not found") }
        }
      }
      paramValue
    }
    requestedParams <- lapply(requestedParams, findParam)

    tuningDescription <- requestedParams$tuningDescription
    for (i in seq_along(requestedParams)) {
      paramDetails <- Find(function(p) identical(p$parameterName, names(requestedParams)[[i]]),
                           availableParams)
      if (!is.null(paramDetails$parameterId)) {
        sentParams <- append(sentParams,
                             list(list("parameterId" = paramDetails$parameterId,
                                       "value" = requestedParams[[i]])))
      }
    }
    if (identical(tuningDescription, "") || is.null(tuningDescription)) {
      payload <- list("tuningParameters" = sentParams)
    } else {
      payload <- list("tuningDescription" = tuningDescription,
                      "tuningParameters" = sentParams)
    }
    routeString <- UrlJoin("projects", model$projectId, "models", model$modelId, "advancedTuning")
    response <- DataRobotPOST(routeString, body = payload,
                              encode = "json", returnRawResponse = TRUE)
    JobIdFromResponse(response)
  }
  formals(tuningFunction) <- args
  tuningFunction
}


# Convenience functions for test mocking
GetUserInput <- function() { readLines(n = 1) }
IsInteractiveMode <- function() { interactive() }

#' Run an interactive model tuning session.
#'
#' The advanced tuning feature allows you to manually set model parameters and override the
#' DataRobot default selections. It is generally available for Eureqa models. To use this
#' feature with other model types, contact your CFDS for more information.
#'
#' This function runs an interactive session to iterate you through individual arguments
#' for each tunable hyperparameter, presenting you with the defaults and other available
#' information. You can set each parameter one at a time, skipping ones you don't intend to
#' set. At the end, it will return a job ID that can be used to get the tuned model.
#'
#' Note that sometimes you may see the exact same parameter more than once. These are for
#' different parts of the blueprint that use the same parameter (e.g., one hot encoding for
#' text and then one hot encoding for numeric). They are listed in the order they are found
#' in the blueprint but unfortunately more user-facing information cannot be provided.
#'
#' @inheritParams GetTuningParameters
#' @return A job ID that can be used to get the tuned model.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   myXGBModel <- GetModel(projectId, modelId)
#'   tuningJob <- RunInteractiveTuning(myXGBModel)
#'   tunedModel <- GetModelFromJobId(projectId, tuningJob)
#' }
#' @export
RunInteractiveTuning <- function(model) {
  if (!IsInteractiveMode()) {
    stop("Interactive tuning can only be run in R interactive mode.")
  }
  availableParams <- GetTuningParameters(model)$tuningParameters
  sentParams <- list()
  for (i in seq_along(availableParams)) {
    param <- availableParams[[i]]
    message("Would you like to set ", param$parameterName, "? Currently ",
            param$currentValue, " (default ", param$defaultValue, "), possible values are ",
            SummarizeConstraints(param$constraints), ". Hit RETURN to skip and keep at ",
            "current value or enter a new value.")
    userInput <- GetUserInput()
    if (!identical(userInput, "")) {
      sentParams <- append(sentParams,
                           list(list("parameterId" = param$parameterId,
                                     "value" = userInput)))
    }
  }
  message("Would you like to describe your tune? Enter a description or just hit RETURN to skip.")
  tuningDescription <- GetUserInput()
  if (identical(tuningDescription, "")) {
    payload <- list("tuningParameters" = sentParams)
  } else {
    payload <- list("tuningDescription" = tuningDescription,
                    "tuningParameters" = sentParams)
  }
  routeString <- UrlJoin("projects", model$projectId, "models", model$modelId, "advancedTuning")
  response <- DataRobotPOST(routeString, body = payload,
                            encode = "json", returnRawResponse = TRUE)
  JobIdFromResponse(response)
}
