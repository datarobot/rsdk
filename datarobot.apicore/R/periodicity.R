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
#' @title Periodicity
#'
#' @description Periodicity Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field timeSteps  integer The number of time steps.
#'
#' @field timeUnit  character The time unit or &#x60;ROW&#x60; if windowsBasisUnit is &#x60;ROW&#x60;
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Periodicity <- R6::R6Class(
  "Periodicity",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`timeSteps` = NULL, `timeUnit` = NULL) {
      if (!is.null(`timeSteps`)) {
        stopifnot(is.numeric(`timeSteps`), length(`timeSteps`) == 1)
      }
      if (!is.null(`timeUnit`)) {
        stopifnot(is.character(`timeUnit`), length(`timeUnit`) == 1)
      }
    }
  ),
  public = list(
    `timeSteps` = NULL,
    `timeUnit` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param timeSteps The number of time steps.
    #' @param timeUnit The time unit or &#x60;ROW&#x60; if windowsBasisUnit is &#x60;ROW&#x60;
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`timeSteps` = NULL, `timeUnit` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`timeSteps`, `timeUnit`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(timeSteps, timeUnit)
      }
      self$`timeSteps` <- `timeSteps`
      self$`timeUnit` <- `timeUnit`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(timeSteps = self$`timeSteps`, timeUnit = self$`timeUnit`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`timeSteps`)) {
          sprintf(
            '"timeSteps":
            %d
                  ',
            self$`timeSteps`
          )
        },
        if (!is.null(self$`timeUnit`)) {
          sprintf(
            '"timeUnit":
            "%s"
                  ',
            self$`timeUnit`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param PeriodicityJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(PeriodicityJson, validateParams = FALSE) {
      PeriodicityObject <- jsonlite::fromJSON(PeriodicityJson)
      self$`timeSteps` <- PeriodicityObject$`timeSteps`
      self$`timeUnit` <- PeriodicityObject$`timeUnit`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
