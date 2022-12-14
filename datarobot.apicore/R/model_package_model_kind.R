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
#' @title ModelPackageModelKind
#'
#' @description ModelPackageModelKind Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field isAnomalyDetectionModel  character true if this is an anomaly detection model
#'
#' @field isCombinedModel  character true if model is a combined model
#'
#' @field isDecisionFlow  character true if this is a decision flow
#'
#' @field isFeatureDiscovery  character true if this model uses the Feature Discovery feature
#'
#' @field isMultiseries  character true if model is multiseries
#'
#' @field isTimeSeries  character true if model is time series
#'
#' @field isUnsupervisedLearning  character true if model used unsupervised learning
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ModelPackageModelKind <- R6::R6Class(
  "ModelPackageModelKind",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`isAnomalyDetectionModel` = NULL, `isCombinedModel` = NULL, `isDecisionFlow` = NULL, `isFeatureDiscovery` = NULL, `isMultiseries` = NULL, `isTimeSeries` = NULL, `isUnsupervisedLearning` = NULL) {
      if (!is.null(`isAnomalyDetectionModel`)) {
        stopifnot(is.logical(`isAnomalyDetectionModel`), length(`isAnomalyDetectionModel`) == 1)
      }
      if (!is.null(`isCombinedModel`)) {
        stopifnot(is.logical(`isCombinedModel`), length(`isCombinedModel`) == 1)
      }
      if (!is.null(`isDecisionFlow`)) {
        stopifnot(is.logical(`isDecisionFlow`), length(`isDecisionFlow`) == 1)
      }
      if (!is.null(`isFeatureDiscovery`)) {
        stopifnot(is.logical(`isFeatureDiscovery`), length(`isFeatureDiscovery`) == 1)
      }
      if (!is.null(`isMultiseries`)) {
        stopifnot(is.logical(`isMultiseries`), length(`isMultiseries`) == 1)
      }
      if (!is.null(`isTimeSeries`)) {
        stopifnot(is.logical(`isTimeSeries`), length(`isTimeSeries`) == 1)
      }
      if (!is.null(`isUnsupervisedLearning`)) {
        stopifnot(is.logical(`isUnsupervisedLearning`), length(`isUnsupervisedLearning`) == 1)
      }
    }
  ),
  public = list(
    `isAnomalyDetectionModel` = NULL,
    `isCombinedModel` = NULL,
    `isDecisionFlow` = NULL,
    `isFeatureDiscovery` = NULL,
    `isMultiseries` = NULL,
    `isTimeSeries` = NULL,
    `isUnsupervisedLearning` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param isAnomalyDetectionModel true if this is an anomaly detection model
    #' @param isCombinedModel true if model is a combined model
    #' @param isDecisionFlow true if this is a decision flow
    #' @param isFeatureDiscovery true if this model uses the Feature Discovery feature
    #' @param isMultiseries true if model is multiseries
    #' @param isTimeSeries true if model is time series
    #' @param isUnsupervisedLearning true if model used unsupervised learning
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`isAnomalyDetectionModel` = NULL, `isCombinedModel` = NULL, `isDecisionFlow` = NULL, `isFeatureDiscovery` = NULL, `isMultiseries` = NULL, `isTimeSeries` = NULL, `isUnsupervisedLearning` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`isAnomalyDetectionModel`, `isCombinedModel`, `isDecisionFlow`, `isFeatureDiscovery`, `isMultiseries`, `isTimeSeries`, `isUnsupervisedLearning`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(isAnomalyDetectionModel, isCombinedModel, isDecisionFlow, isFeatureDiscovery, isMultiseries, isTimeSeries, isUnsupervisedLearning)
      }
      self$`isAnomalyDetectionModel` <- `isAnomalyDetectionModel`
      self$`isCombinedModel` <- `isCombinedModel`
      self$`isDecisionFlow` <- `isDecisionFlow`
      self$`isFeatureDiscovery` <- `isFeatureDiscovery`
      self$`isMultiseries` <- `isMultiseries`
      self$`isTimeSeries` <- `isTimeSeries`
      self$`isUnsupervisedLearning` <- `isUnsupervisedLearning`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(isAnomalyDetectionModel = self$`isAnomalyDetectionModel`, isCombinedModel = self$`isCombinedModel`, isDecisionFlow = self$`isDecisionFlow`, isFeatureDiscovery = self$`isFeatureDiscovery`, isMultiseries = self$`isMultiseries`, isTimeSeries = self$`isTimeSeries`, isUnsupervisedLearning = self$`isUnsupervisedLearning`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`isAnomalyDetectionModel`)) {
          sprintf(
            '"isAnomalyDetectionModel":
            %s
                  ',
            tolower(self$`isAnomalyDetectionModel`)
          )
        },
        if (!is.null(self$`isCombinedModel`)) {
          sprintf(
            '"isCombinedModel":
            %s
                  ',
            tolower(self$`isCombinedModel`)
          )
        },
        if (!is.null(self$`isDecisionFlow`)) {
          sprintf(
            '"isDecisionFlow":
            %s
                  ',
            tolower(self$`isDecisionFlow`)
          )
        },
        if (!is.null(self$`isFeatureDiscovery`)) {
          sprintf(
            '"isFeatureDiscovery":
            %s
                  ',
            tolower(self$`isFeatureDiscovery`)
          )
        },
        if (!is.null(self$`isMultiseries`)) {
          sprintf(
            '"isMultiseries":
            %s
                  ',
            tolower(self$`isMultiseries`)
          )
        },
        if (!is.null(self$`isTimeSeries`)) {
          sprintf(
            '"isTimeSeries":
            %s
                  ',
            tolower(self$`isTimeSeries`)
          )
        },
        if (!is.null(self$`isUnsupervisedLearning`)) {
          sprintf(
            '"isUnsupervisedLearning":
            %s
                  ',
            tolower(self$`isUnsupervisedLearning`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ModelPackageModelKindJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ModelPackageModelKindJson, validateParams = FALSE) {
      ModelPackageModelKindObject <- jsonlite::fromJSON(ModelPackageModelKindJson)
      self$`isAnomalyDetectionModel` <- ModelPackageModelKindObject$`isAnomalyDetectionModel`
      self$`isCombinedModel` <- ModelPackageModelKindObject$`isCombinedModel`
      self$`isDecisionFlow` <- ModelPackageModelKindObject$`isDecisionFlow`
      self$`isFeatureDiscovery` <- ModelPackageModelKindObject$`isFeatureDiscovery`
      self$`isMultiseries` <- ModelPackageModelKindObject$`isMultiseries`
      self$`isTimeSeries` <- ModelPackageModelKindObject$`isTimeSeries`
      self$`isUnsupervisedLearning` <- ModelPackageModelKindObject$`isUnsupervisedLearning`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
