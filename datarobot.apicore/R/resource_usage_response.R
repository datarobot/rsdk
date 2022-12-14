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
#' @title ResourceUsageResponse
#'
#' @description ResourceUsageResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field count  integer The number of resource usage records.
#'
#' @field next_  character URL pointing to the next page.
#'
#' @field previous  character URL pointing to the previous page.
#'
#' @field usage  list( \link{ResourceUsage} ) The resource usage records.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ResourceUsageResponse <- R6::R6Class(
  "ResourceUsageResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`count` = NULL, `next_` = NULL, `previous` = NULL, `usage` = NULL) {
      if (!is.null(`count`)) {
        stopifnot(is.numeric(`count`), length(`count`) == 1)
      }
      if (!is.null(`next_`)) {
        stopifnot(is.character(`next_`), length(`next_`) == 1)
      }
      if (!is.null(`previous`)) {
        stopifnot(is.character(`previous`), length(`previous`) == 1)
      }
      if (!is.null(`usage`)) {
        stopifnot(is.vector(`usage`), sapply(`usage`, R6::is.R6))
      }
    }
  ),
  public = list(
    `count` = NULL,
    `next_` = NULL,
    `previous` = NULL,
    `usage` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param count The number of resource usage records.
    #' @param next_ URL pointing to the next page.
    #' @param previous URL pointing to the previous page.
    #' @param usage The resource usage records.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`count` = NULL, `next_` = NULL, `previous` = NULL, `usage` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`count`, `next_`, `previous`, `usage`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(count, next_, previous, usage)
      }
      self$`count` <- `count`
      self$`next_` <- `next_`
      self$`previous` <- `previous`
      self$`usage` <- `usage`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(count = self$`count`, next_ = self$`next_`, previous = self$`previous`, usage = self$`usage`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`count`)) {
          sprintf(
            '"count":
            %d
                  ',
            self$`count`
          )
        },
        if (!is.null(self$`next_`)) {
          sprintf(
            '"next":
            "%s"
                  ',
            self$`next_`
          )
        },
        if (!is.null(self$`previous`)) {
          sprintf(
            '"previous":
            "%s"
                  ',
            self$`previous`
          )
        },
        if (!is.null(self$`usage`)) {
          sprintf(
            '"usage":
            [%s]
      ',
            paste(sapply(self$`usage`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ResourceUsageResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ResourceUsageResponseJson, validateParams = FALSE) {
      ResourceUsageResponseObject <- jsonlite::fromJSON(ResourceUsageResponseJson)
      self$`count` <- ResourceUsageResponseObject$`count`
      self$`next_` <- ResourceUsageResponseObject$`next`
      self$`previous` <- ResourceUsageResponseObject$`previous`
      self$`usage` <- ApiClient$new()$deserializeObj(ResourceUsageResponseObject$`usage`, "array[ResourceUsage]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
