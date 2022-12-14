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
#' @title ActualValueColumnInfo
#'
#' @description ActualValueColumnInfo Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field missingCount  integer Count of the missing values in the column.
#'
#' @field name  character Name of the column.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ActualValueColumnInfo <- R6::R6Class(
  "ActualValueColumnInfo",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`missingCount` = NULL, `name` = NULL) {
      if (!is.null(`missingCount`)) {
        stopifnot(is.numeric(`missingCount`), length(`missingCount`) == 1)
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
    }
  ),
  public = list(
    `missingCount` = NULL,
    `name` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param missingCount Count of the missing values in the column.
    #' @param name Name of the column.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`missingCount` = NULL, `name` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`missingCount`, `name`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(missingCount, name)
      }
      self$`missingCount` <- `missingCount`
      self$`name` <- `name`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(missingCount = self$`missingCount`, name = self$`name`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`missingCount`)) {
          sprintf(
            '"missingCount":
            %d
                  ',
            self$`missingCount`
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
            '"name":
            "%s"
                  ',
            self$`name`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ActualValueColumnInfoJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ActualValueColumnInfoJson, validateParams = FALSE) {
      ActualValueColumnInfoObject <- jsonlite::fromJSON(ActualValueColumnInfoJson)
      self$`missingCount` <- ActualValueColumnInfoObject$`missingCount`
      self$`name` <- ActualValueColumnInfoObject$`name`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
