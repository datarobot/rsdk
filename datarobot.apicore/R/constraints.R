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
#' @title Constraints
#'
#' @description Constraints Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field ascii  \link{BaseConstraintType} [optional]
#'
#' @field float  \link{Float} [optional]
#'
#' @field floatList  \link{FloatList} [optional]
#'
#' @field int  \link{Int} [optional]
#'
#' @field intList  \link{IntList} [optional]
#'
#' @field select  \link{Select} [optional]
#'
#' @field selectgrid  \link{Select} [optional]
#'
#' @field unicode  \link{BaseConstraintType} [optional]
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Constraints <- R6::R6Class(
  "Constraints",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`ascii` = NULL, `float` = NULL, `floatList` = NULL, `int` = NULL, `intList` = NULL, `select` = NULL, `selectgrid` = NULL, `unicode` = NULL) {
      if (!is.null(`ascii`)) {
        stopifnot(R6::is.R6(`ascii`))
      }
      if (!is.null(`float`)) {
        stopifnot(R6::is.R6(`float`))
      }
      if (!is.null(`floatList`)) {
        stopifnot(R6::is.R6(`floatList`))
      }
      if (!is.null(`int`)) {
        stopifnot(R6::is.R6(`int`))
      }
      if (!is.null(`intList`)) {
        stopifnot(R6::is.R6(`intList`))
      }
      if (!is.null(`select`)) {
        stopifnot(R6::is.R6(`select`))
      }
      if (!is.null(`selectgrid`)) {
        stopifnot(R6::is.R6(`selectgrid`))
      }
      if (!is.null(`unicode`)) {
        stopifnot(R6::is.R6(`unicode`))
      }
    }
  ),
  public = list(
    `ascii` = NULL,
    `float` = NULL,
    `floatList` = NULL,
    `int` = NULL,
    `intList` = NULL,
    `select` = NULL,
    `selectgrid` = NULL,
    `unicode` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param ascii
    #' @param float
    #' @param floatList
    #' @param int
    #' @param intList
    #' @param select
    #' @param selectgrid
    #' @param unicode
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`ascii` = NULL, `float` = NULL, `floatList` = NULL, `int` = NULL, `intList` = NULL, `select` = NULL, `selectgrid` = NULL, `unicode` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(ascii, float, floatList, int, intList, select, selectgrid, unicode)
      }
      self$`ascii` <- `ascii`
      self$`float` <- `float`
      self$`floatList` <- `floatList`
      self$`int` <- `int`
      self$`intList` <- `intList`
      self$`select` <- `select`
      self$`selectgrid` <- `selectgrid`
      self$`unicode` <- `unicode`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(ascii = self$`ascii`, float = self$`float`, floatList = self$`floatList`, int = self$`int`, intList = self$`intList`, select = self$`select`, selectgrid = self$`selectgrid`, unicode = self$`unicode`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`ascii`)) {
          sprintf(
            '"ascii":
            %s
      ',
            jsonlite::toJSON(self$`ascii`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`float`)) {
          sprintf(
            '"float":
            %s
      ',
            jsonlite::toJSON(self$`float`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`floatList`)) {
          sprintf(
            '"floatList":
            %s
      ',
            jsonlite::toJSON(self$`floatList`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`int`)) {
          sprintf(
            '"int":
            %s
      ',
            jsonlite::toJSON(self$`int`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`intList`)) {
          sprintf(
            '"intList":
            %s
      ',
            jsonlite::toJSON(self$`intList`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`select`)) {
          sprintf(
            '"select":
            %s
      ',
            jsonlite::toJSON(self$`select`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`selectgrid`)) {
          sprintf(
            '"selectgrid":
            %s
      ',
            jsonlite::toJSON(self$`selectgrid`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`unicode`)) {
          sprintf(
            '"unicode":
            %s
      ',
            jsonlite::toJSON(self$`unicode`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ConstraintsJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ConstraintsJson, validateParams = FALSE) {
      ConstraintsObject <- jsonlite::fromJSON(ConstraintsJson)
      self$`ascii` <- BaseConstraintType$new()$fromJSON(jsonlite::toJSON(ConstraintsObject$ascii, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`float` <- Float$new()$fromJSON(jsonlite::toJSON(ConstraintsObject$float, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`floatList` <- FloatList$new()$fromJSON(jsonlite::toJSON(ConstraintsObject$floatList, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`int` <- Int$new()$fromJSON(jsonlite::toJSON(ConstraintsObject$int, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`intList` <- IntList$new()$fromJSON(jsonlite::toJSON(ConstraintsObject$intList, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`select` <- Select$new()$fromJSON(jsonlite::toJSON(ConstraintsObject$select, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`selectgrid` <- Select$new()$fromJSON(jsonlite::toJSON(ConstraintsObject$selectgrid, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`unicode` <- BaseConstraintType$new()$fromJSON(jsonlite::toJSON(ConstraintsObject$unicode, auto_unbox = TRUE, digits = NA, null = "null"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
