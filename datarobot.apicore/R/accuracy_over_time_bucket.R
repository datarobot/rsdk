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
#' @title AccuracyOverTimeBucket
#'
#' @description AccuracyOverTimeBucket Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field period  \link{TimeRange}
#'
#' @field sampleSize  integer Number of predictions used to calculate the metric.
#'
#' @field value  numeric Value of the metric, null if no value.
#'
#' @field valuePerClass  object [optional] A dict keyed by class names with metric calculated for specific classes as values, if targetClasses is set.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
AccuracyOverTimeBucket <- R6::R6Class(
  "AccuracyOverTimeBucket",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`period` = NULL, `sampleSize` = NULL, `value` = NULL, `valuePerClass` = NULL) {
      if (!is.null(`period`)) {
        stopifnot(R6::is.R6(`period`))
      }
      if (!is.null(`sampleSize`)) {
        stopifnot(is.numeric(`sampleSize`), length(`sampleSize`) == 1)
      }
      if (!is.null(`value`)) {
      }
      if (!is.null(`valuePerClass`)) {
      }
    }
  ),
  public = list(
    `period` = NULL,
    `sampleSize` = NULL,
    `value` = NULL,
    `valuePerClass` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param period
    #' @param sampleSize Number of predictions used to calculate the metric.
    #' @param value Value of the metric, null if no value.
    #' @param valuePerClass A dict keyed by class names with metric calculated for specific classes as values, if targetClasses is set.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`period` = NULL, `sampleSize` = NULL, `value` = NULL, `valuePerClass` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`period`, `sampleSize`, `value`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(period, sampleSize, value, valuePerClass)
      }
      self$`period` <- `period`
      self$`sampleSize` <- `sampleSize`
      self$`value` <- `value`
      self$`valuePerClass` <- `valuePerClass`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(period = self$`period`, sampleSize = self$`sampleSize`, value = self$`value`, valuePerClass = self$`valuePerClass`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`period`)) {
          sprintf(
            '"period":
            %s
      ',
            jsonlite::toJSON(self$`period`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`sampleSize`)) {
          sprintf(
            '"sampleSize":
            %d
                  ',
            self$`sampleSize`
          )
        },
        if (!is.null(self$`value`)) {
          sprintf(
            '"value":
            %d
                  ',
            self$`value`
          )
        },
        if (!is.null(self$`valuePerClass`)) {
          sprintf(
            '"valuePerClass":
            "%s"
                  ',
            self$`valuePerClass`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param AccuracyOverTimeBucketJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(AccuracyOverTimeBucketJson, validateParams = FALSE) {
      AccuracyOverTimeBucketObject <- jsonlite::fromJSON(AccuracyOverTimeBucketJson)
      self$`period` <- TimeRange$new()$fromJSON(jsonlite::toJSON(AccuracyOverTimeBucketObject$period, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`sampleSize` <- AccuracyOverTimeBucketObject$`sampleSize`
      self$`value` <- AccuracyOverTimeBucketObject$`value`
      self$`valuePerClass` <- AccuracyOverTimeBucketObject$`valuePerClass`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
