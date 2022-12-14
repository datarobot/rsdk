# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

# Public API
#
# DataRobot's Public facing API
#
# The version of the OpenAPI document: 2.29.0
# Contact: api-maintainer@datarobot.com
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title ForecastDistanceStabilityPlotResponse
#'
#' @description ForecastDistanceStabilityPlotResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field endDate  character ISO-formatted start date of the project dataset.
#'
#' @field forecastDistancePlotData  list( \link{ForecastDistancePlotDataEntryResponse} ) An array of objects containing the details of the scores for each forecast distance.
#'
#' @field metricName  character Name of the metric used to compute the scores.
#'
#' @field startDate  character ISO-formatted start date of the project dataset.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ForecastDistanceStabilityPlotResponse <- R6::R6Class(
  "ForecastDistanceStabilityPlotResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`endDate` = NULL, `forecastDistancePlotData` = NULL, `metricName` = NULL, `startDate` = NULL) {
      if (!is.null(`endDate`)) {
        stopifnot(inherits(`endDate`, "POSIXt"))
      }
      if (!is.null(`forecastDistancePlotData`)) {
        stopifnot(is.vector(`forecastDistancePlotData`), sapply(`forecastDistancePlotData`, R6::is.R6))
      }
      if (!is.null(`metricName`)) {
        stopifnot(is.character(`metricName`), length(`metricName`) == 1)
      }
      if (!is.null(`startDate`)) {
        stopifnot(inherits(`startDate`, "POSIXt"))
      }
    }
  ),
  public = list(
    `endDate` = NULL,
    `forecastDistancePlotData` = NULL,
    `metricName` = NULL,
    `startDate` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param endDate ISO-formatted start date of the project dataset.
    #' @param forecastDistancePlotData An array of objects containing the details of the scores for each forecast distance.
    #' @param metricName Name of the metric used to compute the scores.
    #' @param startDate ISO-formatted start date of the project dataset.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`endDate` = NULL, `forecastDistancePlotData` = NULL, `metricName` = NULL, `startDate` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`endDate`, `forecastDistancePlotData`, `metricName`, `startDate`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(endDate, forecastDistancePlotData, metricName, startDate)
      }
      self$`endDate` <- `endDate`
      self$`forecastDistancePlotData` <- `forecastDistancePlotData`
      self$`metricName` <- `metricName`
      self$`startDate` <- `startDate`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(endDate = self$`endDate`, forecastDistancePlotData = self$`forecastDistancePlotData`, metricName = self$`metricName`, startDate = self$`startDate`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`endDate`)) {
          sprintf(
            '"endDate":
            "%s"
                  ',
            format(self$`endDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`forecastDistancePlotData`)) {
          sprintf(
            '"forecastDistancePlotData":
            [%s]
      ',
            paste(sapply(self$`forecastDistancePlotData`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`metricName`)) {
          sprintf(
            '"metricName":
            "%s"
                  ',
            self$`metricName`
          )
        },
        if (!is.null(self$`startDate`)) {
          sprintf(
            '"startDate":
            "%s"
                  ',
            format(self$`startDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ForecastDistanceStabilityPlotResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ForecastDistanceStabilityPlotResponseJson, validateParams = FALSE) {
      ForecastDistanceStabilityPlotResponseObject <- jsonlite::fromJSON(ForecastDistanceStabilityPlotResponseJson)
      self$`endDate` <- ParseRFC3339Timestamp(ForecastDistanceStabilityPlotResponseObject$`endDate`)
      self$`forecastDistancePlotData` <- ApiClient$new()$deserializeObj(ForecastDistanceStabilityPlotResponseObject$`forecastDistancePlotData`, "array[ForecastDistancePlotDataEntryResponse]", loadNamespace("datarobot.apicore"))
      self$`metricName` <- ForecastDistanceStabilityPlotResponseObject$`metricName`
      self$`startDate` <- ParseRFC3339Timestamp(ForecastDistanceStabilityPlotResponseObject$`startDate`)

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
