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
#' @title Backtest
#'
#' @description Backtest Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field gapDuration  character [optional] A duration string representing the duration of the gap between the training and the validation data for this backtest.
#'
#' @field index  integer The index from zero of the backtest specified by this object.
#'
#' @field primaryTrainingEndDate  character [optional] A datetime string representing the end date of the primary training data for this backtest.
#'
#' @field primaryTrainingStartDate  character [optional] A datetime string representing the start date of the primary training data for this backtest.
#'
#' @field validationDuration  character [optional] A duration string representing the duration of the validation data for this backtest.
#'
#' @field validationEndDate  character [optional] A datetime string representing the end date of the validation data for this backtest.
#'
#' @field validationStartDate  character A datetime string representing the start date of the validation data for this backtest.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Backtest <- R6::R6Class(
  "Backtest",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`gapDuration` = NULL, `index` = NULL, `primaryTrainingEndDate` = NULL, `primaryTrainingStartDate` = NULL, `validationDuration` = NULL, `validationEndDate` = NULL, `validationStartDate` = NULL) {
      if (!is.null(`index`)) {
        stopifnot(is.numeric(`index`), length(`index`) == 1)
      }
      if (!is.null(`validationStartDate`)) {
        stopifnot(inherits(`validationStartDate`, "POSIXt"))
      }
      if (!is.null(`gapDuration`)) {
        stopifnot(is.character(`gapDuration`), length(`gapDuration`) == 1)
      }
      if (!is.null(`primaryTrainingEndDate`)) {
        stopifnot(inherits(`primaryTrainingEndDate`, "POSIXt"))
      }
      if (!is.null(`primaryTrainingStartDate`)) {
        stopifnot(inherits(`primaryTrainingStartDate`, "POSIXt"))
      }
      if (!is.null(`validationDuration`)) {
        stopifnot(is.character(`validationDuration`), length(`validationDuration`) == 1)
      }
      if (!is.null(`validationEndDate`)) {
        stopifnot(inherits(`validationEndDate`, "POSIXt"))
      }
    }
  ),
  public = list(
    `gapDuration` = NULL,
    `index` = NULL,
    `primaryTrainingEndDate` = NULL,
    `primaryTrainingStartDate` = NULL,
    `validationDuration` = NULL,
    `validationEndDate` = NULL,
    `validationStartDate` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param gapDuration A duration string representing the duration of the gap between the training and the validation data for this backtest.
    #' @param index The index from zero of the backtest specified by this object.
    #' @param primaryTrainingEndDate A datetime string representing the end date of the primary training data for this backtest.
    #' @param primaryTrainingStartDate A datetime string representing the start date of the primary training data for this backtest.
    #' @param validationDuration A duration string representing the duration of the validation data for this backtest.
    #' @param validationEndDate A datetime string representing the end date of the validation data for this backtest.
    #' @param validationStartDate A datetime string representing the start date of the validation data for this backtest.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`index` = NULL, `validationStartDate` = NULL, `gapDuration` = NULL, `primaryTrainingEndDate` = NULL, `primaryTrainingStartDate` = NULL, `validationDuration` = NULL, `validationEndDate` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`index`, `validationStartDate`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(gapDuration, index, primaryTrainingEndDate, primaryTrainingStartDate, validationDuration, validationEndDate, validationStartDate)
      }
      self$`gapDuration` <- `gapDuration`
      self$`index` <- `index`
      self$`primaryTrainingEndDate` <- `primaryTrainingEndDate`
      self$`primaryTrainingStartDate` <- `primaryTrainingStartDate`
      self$`validationDuration` <- `validationDuration`
      self$`validationEndDate` <- `validationEndDate`
      self$`validationStartDate` <- `validationStartDate`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(gapDuration = self$`gapDuration`, index = self$`index`, primaryTrainingEndDate = self$`primaryTrainingEndDate`, primaryTrainingStartDate = self$`primaryTrainingStartDate`, validationDuration = self$`validationDuration`, validationEndDate = self$`validationEndDate`, validationStartDate = self$`validationStartDate`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`gapDuration`)) {
          sprintf(
            '"gapDuration":
            "%s"
                  ',
            self$`gapDuration`
          )
        },
        if (!is.null(self$`index`)) {
          sprintf(
            '"index":
            %d
                  ',
            self$`index`
          )
        },
        if (!is.null(self$`primaryTrainingEndDate`)) {
          sprintf(
            '"primaryTrainingEndDate":
            "%s"
                  ',
            format(self$`primaryTrainingEndDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`primaryTrainingStartDate`)) {
          sprintf(
            '"primaryTrainingStartDate":
            "%s"
                  ',
            format(self$`primaryTrainingStartDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`validationDuration`)) {
          sprintf(
            '"validationDuration":
            "%s"
                  ',
            self$`validationDuration`
          )
        },
        if (!is.null(self$`validationEndDate`)) {
          sprintf(
            '"validationEndDate":
            "%s"
                  ',
            format(self$`validationEndDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`validationStartDate`)) {
          sprintf(
            '"validationStartDate":
            "%s"
                  ',
            format(self$`validationStartDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param BacktestJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(BacktestJson, validateParams = FALSE) {
      BacktestObject <- jsonlite::fromJSON(BacktestJson)
      self$`gapDuration` <- BacktestObject$`gapDuration`
      self$`index` <- BacktestObject$`index`
      self$`primaryTrainingEndDate` <- ParseRFC3339Timestamp(BacktestObject$`primaryTrainingEndDate`)
      self$`primaryTrainingStartDate` <- ParseRFC3339Timestamp(BacktestObject$`primaryTrainingStartDate`)
      self$`validationDuration` <- BacktestObject$`validationDuration`
      self$`validationEndDate` <- ParseRFC3339Timestamp(BacktestObject$`validationEndDate`)
      self$`validationStartDate` <- ParseRFC3339Timestamp(BacktestObject$`validationStartDate`)

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
