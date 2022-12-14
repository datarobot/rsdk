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
#' @title SampleSize
#'
#' @description SampleSize Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field type  character Sample size can be specified only as a number of rows for now.
#'
#' @field value  integer Number of rows to ingest during dataset registration.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SampleSize <- R6::R6Class(
  "SampleSize",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`type` = NULL, `value` = NULL) {
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
      }
      if (!is.null(`value`)) {
        stopifnot(is.numeric(`value`), length(`value`) == 1)
      }
    }
  ),
  public = list(
    `type` = NULL,
    `value` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param type Sample size can be specified only as a number of rows for now.
    #' @param value Number of rows to ingest during dataset registration.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`type` = NULL, `value` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`type`, `value`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(type, value)
      }
      self$`type` <- `type`
      self$`value` <- `value`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(type = self$`type`, value = self$`value`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`type`)) {
          sprintf(
            '"type":
            "%s"
                  ',
            self$`type`
          )
        },
        if (!is.null(self$`value`)) {
          sprintf(
            '"value":
            %d
                  ',
            self$`value`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param SampleSizeJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(SampleSizeJson, validateParams = FALSE) {
      SampleSizeObject <- jsonlite::fromJSON(SampleSizeJson)
      self$`type` <- SampleSizeObject$`type`
      self$`value` <- SampleSizeObject$`value`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
