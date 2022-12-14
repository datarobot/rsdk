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
#' @title BatchPredictionJobTimeSeriesSettingsForecastWithPolicy
#'
#' @description BatchPredictionJobTimeSeriesSettingsForecastWithPolicy Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field forecastPointPolicy  \link{JobRunTimeBasedForecastPointPolicy}
#'
#' @field relaxKnownInAdvanceFeaturesCheck  character [optional] If activated, missing values in the known in advance features are allowed in the forecast window at prediction time. If omitted or false, missing values are not allowed.
#'
#' @field type  character Forecast mode makes predictions using forecastPoint or rows in the dataset without target.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
BatchPredictionJobTimeSeriesSettingsForecastWithPolicy <- R6::R6Class(
  "BatchPredictionJobTimeSeriesSettingsForecastWithPolicy",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`forecastPointPolicy` = NULL, `relaxKnownInAdvanceFeaturesCheck` = NULL, `type` = NULL) {
      if (!is.null(`forecastPointPolicy`)) {
        stopifnot(R6::is.R6(`forecastPointPolicy`))
      }
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
      }
      if (!is.null(`relaxKnownInAdvanceFeaturesCheck`)) {
        stopifnot(is.logical(`relaxKnownInAdvanceFeaturesCheck`), length(`relaxKnownInAdvanceFeaturesCheck`) == 1)
      }
    }
  ),
  public = list(
    `forecastPointPolicy` = NULL,
    `relaxKnownInAdvanceFeaturesCheck` = NULL,
    `type` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param forecastPointPolicy
    #' @param relaxKnownInAdvanceFeaturesCheck If activated, missing values in the known in advance features are allowed in the forecast window at prediction time. If omitted or false, missing values are not allowed.
    #' @param type Forecast mode makes predictions using forecastPoint or rows in the dataset without target.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`forecastPointPolicy` = NULL, `type` = NULL, `relaxKnownInAdvanceFeaturesCheck` = FALSE, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`forecastPointPolicy`, `type`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(forecastPointPolicy, relaxKnownInAdvanceFeaturesCheck, type)
      }
      self$`forecastPointPolicy` <- `forecastPointPolicy`
      self$`relaxKnownInAdvanceFeaturesCheck` <- `relaxKnownInAdvanceFeaturesCheck`
      self$`type` <- `type`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(forecastPointPolicy = self$`forecastPointPolicy`, relaxKnownInAdvanceFeaturesCheck = self$`relaxKnownInAdvanceFeaturesCheck`, type = self$`type`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`forecastPointPolicy`)) {
          sprintf(
            '"forecastPointPolicy":
            %s
      ',
            jsonlite::toJSON(self$`forecastPointPolicy`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`relaxKnownInAdvanceFeaturesCheck`)) {
          sprintf(
            '"relaxKnownInAdvanceFeaturesCheck":
            %s
                  ',
            tolower(self$`relaxKnownInAdvanceFeaturesCheck`)
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
            '"type":
            "%s"
                  ',
            self$`type`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param BatchPredictionJobTimeSeriesSettingsForecastWithPolicyJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(BatchPredictionJobTimeSeriesSettingsForecastWithPolicyJson, validateParams = FALSE) {
      BatchPredictionJobTimeSeriesSettingsForecastWithPolicyObject <- jsonlite::fromJSON(BatchPredictionJobTimeSeriesSettingsForecastWithPolicyJson)
      self$`forecastPointPolicy` <- JobRunTimeBasedForecastPointPolicy$new()$fromJSON(jsonlite::toJSON(BatchPredictionJobTimeSeriesSettingsForecastWithPolicyObject$forecastPointPolicy, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`relaxKnownInAdvanceFeaturesCheck` <- BatchPredictionJobTimeSeriesSettingsForecastWithPolicyObject$`relaxKnownInAdvanceFeaturesCheck`
      self$`type` <- BatchPredictionJobTimeSeriesSettingsForecastWithPolicyObject$`type`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
