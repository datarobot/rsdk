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
#' @title BiasVsAccuracyInsight
#'
#' @description BiasVsAccuracyInsight Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field accuracyMetric  character [optional] The metric to return model accuracy scores. Defaults to the optimization metric configured in project options.
#'
#' @field fairnessMetric  \link{OneOfstringarray} [optional] The fairness metric used to calculate the fairness scores.
#'
#' @field fairnessThreshold  numeric Value of the fairness threshold, defined in project options.
#'
#' @field models  list( \link{BiasVsAccuracyModels} ) An array of models of the insight.
#'
#' @field protectedFeature  \link{OneOfstringarray} [optional] Name of the protected feature.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
BiasVsAccuracyInsight <- R6::R6Class(
  "BiasVsAccuracyInsight",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`accuracyMetric` = NULL, `fairnessMetric` = NULL, `fairnessThreshold` = NULL, `models` = NULL, `protectedFeature` = NULL) {
      if (!is.null(`fairnessThreshold`)) {
      }
      if (!is.null(`models`)) {
        stopifnot(is.vector(`models`), sapply(`models`, R6::is.R6))
      }
      if (!is.null(`accuracyMetric`)) {
        stopifnot(is.character(`accuracyMetric`), length(`accuracyMetric`) == 1)
      }
      if (!is.null(`fairnessMetric`)) {
        .setPrimitiveProperty(typeList = list("character", "array"), propertyData = fairnessMetric)
      }
      if (!is.null(`protectedFeature`)) {
        .setPrimitiveProperty(typeList = list("character", "array"), propertyData = protectedFeature)
      }
    }
  ),
  public = list(
    `accuracyMetric` = NULL,
    `fairnessMetric` = NULL,
    `fairnessThreshold` = NULL,
    `models` = NULL,
    `protectedFeature` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param accuracyMetric The metric to return model accuracy scores. Defaults to the optimization metric configured in project options.
    #' @param fairnessMetric The fairness metric used to calculate the fairness scores.
    #' @param fairnessThreshold Value of the fairness threshold, defined in project options.
    #' @param models An array of models of the insight.
    #' @param protectedFeature Name of the protected feature.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`fairnessThreshold` = NULL, `models` = NULL, `accuracyMetric` = NULL, `fairnessMetric` = NULL, `protectedFeature` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`fairnessThreshold`, `models`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(accuracyMetric, fairnessMetric, fairnessThreshold, models, protectedFeature)
      }
      self$`accuracyMetric` <- `accuracyMetric`
      self$`fairnessMetric` <- .setPrimitiveProperty(typeList = list("character", "array"), propertyData = fairnessMetric)
      self$`fairnessThreshold` <- `fairnessThreshold`
      self$`models` <- `models`
      self$`protectedFeature` <- .setPrimitiveProperty(typeList = list("character", "array"), propertyData = protectedFeature)
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(accuracyMetric = self$`accuracyMetric`, fairnessMetric = self$`fairnessMetric`, fairnessThreshold = self$`fairnessThreshold`, models = self$`models`, protectedFeature = self$`protectedFeature`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`accuracyMetric`)) {
          sprintf(
            '"accuracyMetric":
            "%s"
                  ',
            self$`accuracyMetric`
          )
        },
        if (!is.null(self$`fairnessMetric`)) {
          sprintf(
            '"fairnessMetric":
            %s
      ',
            self$`fairnessMetric`
          )
        },
        if (!is.null(self$`fairnessThreshold`)) {
          sprintf(
            '"fairnessThreshold":
            %d
                  ',
            self$`fairnessThreshold`
          )
        },
        if (!is.null(self$`models`)) {
          sprintf(
            '"models":
            [%s]
      ',
            paste(sapply(self$`models`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`protectedFeature`)) {
          sprintf(
            '"protectedFeature":
            %s
      ',
            self$`protectedFeature`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param BiasVsAccuracyInsightJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(BiasVsAccuracyInsightJson, validateParams = FALSE) {
      BiasVsAccuracyInsightObject <- jsonlite::fromJSON(BiasVsAccuracyInsightJson)
      self$`accuracyMetric` <- BiasVsAccuracyInsightObject$`accuracyMetric`
      self$`fairnessMetric` <- .setPrimitiveProperty(typeList = list("character", "array"), propertyData = BiasVsAccuracyInsightObject$fairnessMetric)
      self$`fairnessThreshold` <- BiasVsAccuracyInsightObject$`fairnessThreshold`
      self$`models` <- ApiClient$new()$deserializeObj(BiasVsAccuracyInsightObject$`models`, "array[BiasVsAccuracyModels]", loadNamespace("datarobot.apicore"))
      self$`protectedFeature` <- .setPrimitiveProperty(typeList = list("character", "array"), propertyData = BiasVsAccuracyInsightObject$protectedFeature)

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
