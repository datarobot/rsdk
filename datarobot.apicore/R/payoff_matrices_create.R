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
#' @title PayoffMatricesCreate
#'
#' @description PayoffMatricesCreate Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field falseNegativeValue  numeric False negative value to use for profit curve calculation.
#'
#' @field falsePositiveValue  numeric False positive value to use for profit curve calculation.
#'
#' @field name  character Name of the payoff matrix to be created.
#'
#' @field trueNegativeValue  numeric True negative value to use for profit curve calculation.
#'
#' @field truePositiveValue  numeric True positive value to use for profit curve calculation.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PayoffMatricesCreate <- R6::R6Class(
  "PayoffMatricesCreate",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`falseNegativeValue` = NULL, `falsePositiveValue` = NULL, `name` = NULL, `trueNegativeValue` = NULL, `truePositiveValue` = NULL) {
      if (!is.null(`falseNegativeValue`)) {
      }
      if (!is.null(`falsePositiveValue`)) {
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`trueNegativeValue`)) {
      }
      if (!is.null(`truePositiveValue`)) {
      }
    }
  ),
  public = list(
    `falseNegativeValue` = NULL,
    `falsePositiveValue` = NULL,
    `name` = NULL,
    `trueNegativeValue` = NULL,
    `truePositiveValue` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param falseNegativeValue False negative value to use for profit curve calculation.
    #' @param falsePositiveValue False positive value to use for profit curve calculation.
    #' @param name Name of the payoff matrix to be created.
    #' @param trueNegativeValue True negative value to use for profit curve calculation.
    #' @param truePositiveValue True positive value to use for profit curve calculation.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`falseNegativeValue` = NULL, `falsePositiveValue` = NULL, `name` = NULL, `trueNegativeValue` = NULL, `truePositiveValue` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`falseNegativeValue`, `falsePositiveValue`, `name`, `trueNegativeValue`, `truePositiveValue`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(falseNegativeValue, falsePositiveValue, name, trueNegativeValue, truePositiveValue)
      }
      self$`falseNegativeValue` <- `falseNegativeValue`
      self$`falsePositiveValue` <- `falsePositiveValue`
      self$`name` <- `name`
      self$`trueNegativeValue` <- `trueNegativeValue`
      self$`truePositiveValue` <- `truePositiveValue`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(falseNegativeValue = self$`falseNegativeValue`, falsePositiveValue = self$`falsePositiveValue`, name = self$`name`, trueNegativeValue = self$`trueNegativeValue`, truePositiveValue = self$`truePositiveValue`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`falseNegativeValue`)) {
          sprintf(
            '"falseNegativeValue":
            %d
                  ',
            self$`falseNegativeValue`
          )
        },
        if (!is.null(self$`falsePositiveValue`)) {
          sprintf(
            '"falsePositiveValue":
            %d
                  ',
            self$`falsePositiveValue`
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
            '"name":
            "%s"
                  ',
            self$`name`
          )
        },
        if (!is.null(self$`trueNegativeValue`)) {
          sprintf(
            '"trueNegativeValue":
            %d
                  ',
            self$`trueNegativeValue`
          )
        },
        if (!is.null(self$`truePositiveValue`)) {
          sprintf(
            '"truePositiveValue":
            %d
                  ',
            self$`truePositiveValue`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param PayoffMatricesCreateJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(PayoffMatricesCreateJson, validateParams = FALSE) {
      PayoffMatricesCreateObject <- jsonlite::fromJSON(PayoffMatricesCreateJson)
      self$`falseNegativeValue` <- PayoffMatricesCreateObject$`falseNegativeValue`
      self$`falsePositiveValue` <- PayoffMatricesCreateObject$`falsePositiveValue`
      self$`name` <- PayoffMatricesCreateObject$`name`
      self$`trueNegativeValue` <- PayoffMatricesCreateObject$`trueNegativeValue`
      self$`truePositiveValue` <- PayoffMatricesCreateObject$`truePositiveValue`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
