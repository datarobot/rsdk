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
#' @title UsersPermadeleteDeleteReportParamsResponse
#'
#' @description UsersPermadeleteDeleteReportParamsResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field data  \link{UsersPermadeleteResponse}
#'
#' @field message  character May contain further details.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
UsersPermadeleteDeleteReportParamsResponse <- R6::R6Class(
  "UsersPermadeleteDeleteReportParamsResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`data` = NULL, `message` = NULL) {
      if (!is.null(`data`)) {
        stopifnot(R6::is.R6(`data`))
      }
      if (!is.null(`message`)) {
        stopifnot(is.character(`message`), length(`message`) == 1)
      }
    }
  ),
  public = list(
    `data` = NULL,
    `message` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param data
    #' @param message May contain further details.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`data` = NULL, `message` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`data`, `message`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(data, message)
      }
      self$`data` <- `data`
      self$`message` <- `message`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(data = self$`data`, message = self$`message`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`data`)) {
          sprintf(
            '"data":
            %s
      ',
            jsonlite::toJSON(self$`data`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`message`)) {
          sprintf(
            '"message":
            "%s"
                  ',
            self$`message`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param UsersPermadeleteDeleteReportParamsResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(UsersPermadeleteDeleteReportParamsResponseJson, validateParams = FALSE) {
      UsersPermadeleteDeleteReportParamsResponseObject <- jsonlite::fromJSON(UsersPermadeleteDeleteReportParamsResponseJson)
      self$`data` <- UsersPermadeleteResponse$new()$fromJSON(jsonlite::toJSON(UsersPermadeleteDeleteReportParamsResponseObject$data, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`message` <- UsersPermadeleteDeleteReportParamsResponseObject$`message`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
