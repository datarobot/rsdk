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
#' @title ApplicationAccessControlUpdateRequest
#'
#' @description ApplicationAccessControlUpdateRequest Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field data  list( \link{ApplicationAccessPermission} ) An array of AccessControlPermissionValidator objects
#'
#' @field permissions  list( \link{ApplicationAccessPermission} ) [optional] The list of permission objects describing which users to modify access for.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ApplicationAccessControlUpdateRequest <- R6::R6Class(
  "ApplicationAccessControlUpdateRequest",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`data` = NULL, `permissions` = NULL) {
      if (!is.null(`data`)) {
        stopifnot(is.vector(`data`), sapply(`data`, R6::is.R6))
      }
      if (!is.null(`permissions`) && length(`permissions`) > 0) {
        stopifnot(is.vector(`permissions`), sapply(`permissions`, R6::is.R6))
      }
    }
  ),
  public = list(
    `data` = NULL,
    `permissions` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param data An array of AccessControlPermissionValidator objects
    #' @param permissions The list of permission objects describing which users to modify access for.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`data` = NULL, `permissions` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`data`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(data, permissions)
      }
      self$`data` <- `data`
      self$`permissions` <- `permissions`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(data = self$`data`, permissions = self$`permissions`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`data`)) {
          sprintf(
            '"data":
            [%s]
      ',
            paste(sapply(self$`data`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`permissions`)) {
          sprintf(
            '"permissions":
            [%s]
      ',
            paste(sapply(self$`permissions`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ApplicationAccessControlUpdateRequestJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ApplicationAccessControlUpdateRequestJson, validateParams = FALSE) {
      ApplicationAccessControlUpdateRequestObject <- jsonlite::fromJSON(ApplicationAccessControlUpdateRequestJson)
      self$`data` <- ApiClient$new()$deserializeObj(ApplicationAccessControlUpdateRequestObject$`data`, "array[ApplicationAccessPermission]", loadNamespace("datarobot.apicore"))
      self$`permissions` <- ApiClient$new()$deserializeObj(ApplicationAccessControlUpdateRequestObject$`permissions`, "array[ApplicationAccessPermission]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
