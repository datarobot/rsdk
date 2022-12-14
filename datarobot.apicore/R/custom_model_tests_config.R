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
#' @title CustomModelTestsConfig
#'
#' @description CustomModelTestsConfig Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field errorCheck  character [optional] Ensures that the model can make predictions on the provided test dataset.
#'
#' @field longRunningService  character [optional] Ensures that the custom model image can build and launch. If it cannot, the test is marked as Failed and subsequent test are aborted.
#'
#' @field nullValueImputation  character [optional] Verifies that the model can impute null values. Required for Feature Impact.
#'
#' @field sideEffects  character [optional] Verifies that predictions made on the dataset match row-wise predictions for the same dataset. Fails if the predictions do not match.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CustomModelTestsConfig <- R6::R6Class(
  "CustomModelTestsConfig",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`errorCheck` = NULL, `longRunningService` = NULL, `nullValueImputation` = NULL, `sideEffects` = NULL) {
      if (!is.null(`errorCheck`)) {
        stopifnot(is.character(`errorCheck`), length(`errorCheck`) == 1)
      }
      if (!is.null(`longRunningService`)) {
        stopifnot(is.character(`longRunningService`), length(`longRunningService`) == 1)
      }
      if (!is.null(`nullValueImputation`)) {
        stopifnot(is.character(`nullValueImputation`), length(`nullValueImputation`) == 1)
      }
      if (!is.null(`sideEffects`)) {
        stopifnot(is.character(`sideEffects`), length(`sideEffects`) == 1)
      }
    }
  ),
  public = list(
    `errorCheck` = NULL,
    `longRunningService` = NULL,
    `nullValueImputation` = NULL,
    `sideEffects` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param errorCheck Ensures that the model can make predictions on the provided test dataset.
    #' @param longRunningService Ensures that the custom model image can build and launch. If it cannot, the test is marked as Failed and subsequent test are aborted.
    #' @param nullValueImputation Verifies that the model can impute null values. Required for Feature Impact.
    #' @param sideEffects Verifies that predictions made on the dataset match row-wise predictions for the same dataset. Fails if the predictions do not match.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`errorCheck` = "fail", `longRunningService` = "fail", `nullValueImputation` = "warn", `sideEffects` = "warn", validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(errorCheck, longRunningService, nullValueImputation, sideEffects)
      }
      self$`errorCheck` <- `errorCheck`
      self$`longRunningService` <- `longRunningService`
      self$`nullValueImputation` <- `nullValueImputation`
      self$`sideEffects` <- `sideEffects`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(errorCheck = self$`errorCheck`, longRunningService = self$`longRunningService`, nullValueImputation = self$`nullValueImputation`, sideEffects = self$`sideEffects`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`errorCheck`)) {
          sprintf(
            '"errorCheck":
            "%s"
                  ',
            self$`errorCheck`
          )
        },
        if (!is.null(self$`longRunningService`)) {
          sprintf(
            '"longRunningService":
            "%s"
                  ',
            self$`longRunningService`
          )
        },
        if (!is.null(self$`nullValueImputation`)) {
          sprintf(
            '"nullValueImputation":
            "%s"
                  ',
            self$`nullValueImputation`
          )
        },
        if (!is.null(self$`sideEffects`)) {
          sprintf(
            '"sideEffects":
            "%s"
                  ',
            self$`sideEffects`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CustomModelTestsConfigJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CustomModelTestsConfigJson, validateParams = FALSE) {
      CustomModelTestsConfigObject <- jsonlite::fromJSON(CustomModelTestsConfigJson)
      self$`errorCheck` <- CustomModelTestsConfigObject$`errorCheck`
      self$`longRunningService` <- CustomModelTestsConfigObject$`longRunningService`
      self$`nullValueImputation` <- CustomModelTestsConfigObject$`nullValueImputation`
      self$`sideEffects` <- CustomModelTestsConfigObject$`sideEffects`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
