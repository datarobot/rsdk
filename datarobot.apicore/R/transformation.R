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
#' @title Transformation
#'
#' @description Transformation Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field enabled  character [optional] Whether this transformation is enabled by default
#'
#' @field name  character Transformation name
#'
#' @field params  list( \link{TransformationParam} ) [optional] Config values for transformation
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Transformation <- R6::R6Class(
  "Transformation",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`enabled` = NULL, `name` = NULL, `params` = NULL) {
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`enabled`)) {
        stopifnot(is.logical(`enabled`), length(`enabled`) == 1)
      }
      if (!is.null(`params`) && length(`params`) > 0) {
        stopifnot(is.vector(`params`), sapply(`params`, R6::is.R6))
      }
    }
  ),
  public = list(
    `enabled` = NULL,
    `name` = NULL,
    `params` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param enabled Whether this transformation is enabled by default
    #' @param name Transformation name
    #' @param params Config values for transformation
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`name` = NULL, `enabled` = FALSE, `params` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`name`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(enabled, name, params)
      }
      self$`enabled` <- `enabled`
      self$`name` <- `name`
      self$`params` <- `params`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(enabled = self$`enabled`, name = self$`name`, params = self$`params`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`enabled`)) {
          sprintf(
            '"enabled":
            %s
                  ',
            tolower(self$`enabled`)
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
        if (!is.null(self$`params`)) {
          sprintf(
            '"params":
            [%s]
      ',
            paste(sapply(self$`params`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param TransformationJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(TransformationJson, validateParams = FALSE) {
      TransformationObject <- jsonlite::fromJSON(TransformationJson)
      self$`enabled` <- TransformationObject$`enabled`
      self$`name` <- TransformationObject$`name`
      self$`params` <- ApiClient$new()$deserializeObj(TransformationObject$`params`, "array[TransformationParam]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
