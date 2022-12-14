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
#' @title ServiceStatsRetrieveResponse
#'
#' @description ServiceStatsRetrieveResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field metrics  \link{ServiceStatsMetrics}
#'
#' @field modelId  character [optional] The id of the model for which metrics are being retrieved.
#'
#' @field period  \link{TimeRange}
#'
#' @field segmentAttribute  character [optional] The name of the segment on which segment analysis is being performed.
#'
#' @field segmentValue  character [optional] The value of the &#x60;segmentAttribute&#x60; to segment on.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ServiceStatsRetrieveResponse <- R6::R6Class(
  "ServiceStatsRetrieveResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`metrics` = NULL, `modelId` = NULL, `period` = NULL, `segmentAttribute` = NULL, `segmentValue` = NULL) {
      if (!is.null(`metrics`)) {
        stopifnot(R6::is.R6(`metrics`))
      }
      if (!is.null(`period`)) {
        stopifnot(R6::is.R6(`period`))
      }
      if (!is.null(`modelId`)) {
        stopifnot(is.character(`modelId`), length(`modelId`) == 1)
      }
      if (!is.null(`segmentAttribute`)) {
        stopifnot(is.character(`segmentAttribute`), length(`segmentAttribute`) == 1)
      }
      if (!is.null(`segmentValue`)) {
        stopifnot(is.character(`segmentValue`), length(`segmentValue`) == 1)
      }
    }
  ),
  public = list(
    `metrics` = NULL,
    `modelId` = NULL,
    `period` = NULL,
    `segmentAttribute` = NULL,
    `segmentValue` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param metrics
    #' @param modelId The id of the model for which metrics are being retrieved.
    #' @param period
    #' @param segmentAttribute The name of the segment on which segment analysis is being performed.
    #' @param segmentValue The value of the &#x60;segmentAttribute&#x60; to segment on.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`metrics` = NULL, `period` = NULL, `modelId` = NULL, `segmentAttribute` = NULL, `segmentValue` = "", validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`metrics`, `period`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(metrics, modelId, period, segmentAttribute, segmentValue)
      }
      self$`metrics` <- `metrics`
      self$`modelId` <- `modelId`
      self$`period` <- `period`
      self$`segmentAttribute` <- `segmentAttribute`
      self$`segmentValue` <- `segmentValue`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(metrics = self$`metrics`, modelId = self$`modelId`, period = self$`period`, segmentAttribute = self$`segmentAttribute`, segmentValue = self$`segmentValue`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`metrics`)) {
          sprintf(
            '"metrics":
            %s
      ',
            jsonlite::toJSON(self$`metrics`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`modelId`)) {
          sprintf(
            '"modelId":
            "%s"
                  ',
            self$`modelId`
          )
        },
        if (!is.null(self$`period`)) {
          sprintf(
            '"period":
            %s
      ',
            jsonlite::toJSON(self$`period`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`segmentAttribute`)) {
          sprintf(
            '"segmentAttribute":
            "%s"
                  ',
            self$`segmentAttribute`
          )
        },
        if (!is.null(self$`segmentValue`)) {
          sprintf(
            '"segmentValue":
            "%s"
                  ',
            self$`segmentValue`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ServiceStatsRetrieveResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ServiceStatsRetrieveResponseJson, validateParams = FALSE) {
      ServiceStatsRetrieveResponseObject <- jsonlite::fromJSON(ServiceStatsRetrieveResponseJson)
      self$`metrics` <- ServiceStatsMetrics$new()$fromJSON(jsonlite::toJSON(ServiceStatsRetrieveResponseObject$metrics, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`modelId` <- ServiceStatsRetrieveResponseObject$`modelId`
      self$`period` <- TimeRange$new()$fromJSON(jsonlite::toJSON(ServiceStatsRetrieveResponseObject$period, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`segmentAttribute` <- ServiceStatsRetrieveResponseObject$`segmentAttribute`
      self$`segmentValue` <- ServiceStatsRetrieveResponseObject$`segmentValue`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
